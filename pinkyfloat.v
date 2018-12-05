//This is the source code for the pipelined PinKY (floating-point capable)
//processor design. added words for push.

//System Level Stuff (word sizes, memory sizes, etc.)
`define WORD  [15:0]
`define REGSIZE [15:0]
`define MEMSIZE [65535:0]
`define PRESIZE [11:0]
`define REG [3:0]

//Instruction Encoding (format of AIK generated assembly code)
`define Opcode [15:11]
`define CC [10:9]
`define AL 2'b00
`define S   2'b01
`define EQ 2'b10
`define NE 2'b11
`define isReg [8]
`define Dest  [7:4]
`define Op2 [3:0]
`define PCWRITE 4'b1111
//defines the instruction fetch operation depending on the value of frz
//frz = 1 when the stage is frozen (register interlock), = 0 when normal operation
`define FRZ_DEP_INSTR instrmem[PC_in1-frz]

//OPcodes w/ matching State #s
`define OPadd 5'b00000
`define OPand 5'b00010
`define OPbic 5'b00011
`define OPeor 5'b00100
`define OPldr 5'b00111
`define OPmov 5'b01000
`define OPmul 5'b01001
`define OPneg 5'b01011
`define OPorr 5'b01100
`define OPsha 5'b01110
`define OPstr 5'b01111
`define OPslt 5'b10000
`define OPsub 5'b10001
`define OPitof 5'b10010
`define OPftoi 5'b10011
`define OPrecf 5'b10100
`define OPmulf 5'b10101
`define OPsubf 5'b10110
`define OPaddf 5'b10111
`define OPsys 5'b11111
`define OPpre 5'b11000

//NOP instruction
`define NOP 16'b1xxxxxxxxxxxxxxx

module lead0s(d, s);
    output reg[4:0] d; input wire[15:0] s;
    reg[7:0] s8; reg[3:0] s4; reg[1:0] s2;
    always @(*) begin
        if (s[15:0] == 0) d = 16; 
        else begin
        d[4] = 0;
        {d[3],s8} = ((|s[15:8]) ? {1'b0,s[15:8]} : {1'b1,s[7:0]});
        {d[2],s4} = ((|s8[7:4]) ? {1'b0,s8[7:4]} : {1'b1,s8[3:0]});
        {d[1],s2} = ((|s4[3:2]) ? {1'b0,s4[3:2]} : {1'b1,s4[1:0]});
        d[0] = !s2[1];
        end
    end
endmodule


//Floating point definition (syntax)
`define BIAS 8'b10000110 //exponent exp = E - bias - 7, so total BIAS = 134
`define SIGN [15] //1 bit leading sign
`define EXP [14:7] //8 bit exponent
`define MANT [6:0] //7 bit mantissa (implied leading 1.mantissa)


`define FPU_START 50
`define FPU_ITOF_S2 51
`define FPU_ITOF_S3 52


//floating point unit
//reg done : 1 when the floating point operation has completed
//reg done : 0 when the floating point operation is currently running
module fpu(input en, input clk, input `WORD op1, input `WORD op2, input [4:0] instr, output reg `WORD result, output reg done);
    // Sign = ((i & 0x8000) ? 1 : 0);
    // Exp = ((i >> 7) & 0xff);
    // Frac = ((i & 0x7f) + (f16 ? 0x80 : 0));
    // if int = 0x0000 then the fraction becomes 0, otherwise normalize with 0x80'
    initial begin
        done <= 1;
    end


    integer state = `FPU_START;
    integer tmp;
    reg sign;
    reg [7:0] exp;
    reg [6:0] frac;
    reg `WORD int;
    wire [4:0] d;
    lead0s lead0(.d(d), .s(int));
    always @(posedge clk) begin
        if (en) begin
            done <= 0; //operation is currently running
            case(state)
                `FPU_START: begin
                    case (instr)
                        `OPitof: begin
                            if(op2) begin           //if op2 ain't 0 
                                sign <= op2[15];
                                if(op2[15]) int <= (~op2) + 1; //possible error: may not execute in 1 clock cycle (2's compliment)
                                else int <= op2;
                                state <= `FPU_ITOF_S2;
                            end else begin          //if we's a dummy and try to convert 0 to float 
                                result <= 0;
                                done <= 1;
                            end
                        end
                        `OPftoi: begin end
                        `OPmulf: begin end
                        `OPrecf: begin end
                        `OPsubf: begin end
                        `OPaddf: begin end
                        default: begin end
                    endcase 
                end
                
                
                
                `FPU_ITOF_S2: begin
                    exp <= 127 + (15 - d);
                    tmp <= (int << (d + 1));
                    state <= `FPU_ITOF_S3;
                end
                `FPU_ITOF_S3: begin 
                    result <= {sign, exp, tmp[15:9]};
                    done <= 1; 
                end
            endcase
        end
    end
endmodule


module processor(halt, reset, clk);
    output reg halt;
    input reset, clk;

    reg `WORD regfile `REGSIZE;
    reg `WORD datamem `MEMSIZE;        //instantiate data memory
    reg `WORD instrmem `MEMSIZE;    //instantiate instruction memory
    reg [7:0] reciprocal_lookup [127:0];
    reg init;
    reg frz;
    reg Zflag;
    reg PREflag; //tells whether the PRE is ready for use
    reg `PRESIZE PREval; //contains the PRE value, if there is one
    reg regWrite; //control mux

    reg `WORD ir_in0, ir_in1, ir_in2, ir_in3;
    reg `WORD PC_in0, PC_in1, PC_in2, PC_in3;
    reg `WORD outputVal; //output from the ALU or MEM
    reg `WORD op1, op2;

    reg fpu_en = 0; wire fpu_done = 1; wire `WORD fpu_result;
//module fpu(input en, input clk, input `WORD op1, input `WORD op2, input [4:0] instr, output reg `WORD result, output reg done);
    fpu myfpu(.en(fpu_en), .clk(clk), .op1(op1), .op2(op2), .instr(ir_in3 `Opcode), .result(fpu_result), .done(fpu_done));

    always @(reset) begin
        halt = 0;
        init = 1;
        frz = 0;
        Zflag = 0;
        PC_in0=0;
        PREval=0;
        PREflag=0;
        fpu_en = 0;
        $readmemh("instruction.mem", instrmem);          
        $readmemh("regfile.mem", regfile);               
        //$readmemh2(datamem);        
        $readmemh("reciprocal_look.mem", reciprocal_lookup);
    end
       
   
/*
Stage 0 (owns PC)
-Determines the value of PC
-Sets the Z flag
-Performs writes to the regfile
*/
    always @(posedge clk) begin
        if(fpu_done && !fpu_en) begin
            //writing to the register
            //condition: processor not being initialized, regWrite = 1, in_in0 is not jump
            if ((!init) && regWrite && !(ir_in0 `Dest == `PCWRITE)) regfile[ir_in0 `Dest] = outputVal;

            //setting the Z-flag
            if ((ir_in0 `CC == `S) && (ir_in0[15:12]!=`OPpre) && (!init)) Zflag = !outputVal;

            if(ir_in0 `Dest == `PCWRITE) begin //processing JUMPS
                PC_in0 <= outputVal+1;
                PC_in1 <= outputVal;
                regfile[15] <=outputVal;
                frz<=0;
            end else if(!frz) begin    //processing regular instructions that aren't jumps
                PC_in0 <= PC_in0 + 1;
                PC_in1 <= PC_in0;
                if(PC_in3===16'bxxxxxxxxxxxxxxxx) begin
                    regfile[15]<=0;    
                end else begin
                    regfile[15] <= PC_in3+1; //setting the pc
                end
            end //end else if
        end
        
    end

/*
Stage 1
-fetch an instruction from memory
-set PREval
-checks if the Zflag matches an EQ or NE instruction
*/

    always @(posedge clk) begin
        if(fpu_done && !fpu_en) begin
            //These are the dependencies we check for. If there is a dependency, we send NOPs until it is resolved.
            if (`FRZ_DEP_INSTR[15:12] == `OPpre) begin
                PREval<=`FRZ_DEP_INSTR[11:0];
                frz <= 0; 
                if( !((`FRZ_DEP_INSTR `CC == `EQ && Zflag==0) || (`FRZ_DEP_INSTR `CC == `NE && Zflag==1))) begin
                    ir_in2 <= `FRZ_DEP_INSTR;
                    PC_in2 <= PC_in1;
                end
            end else if ((ir_in3 `Dest == `PCWRITE) ||
                (ir_in2 `Dest == `PCWRITE) || 
                (ir_in0 `Dest == `PCWRITE) || 
                ((`FRZ_DEP_INSTR `isReg) && (`FRZ_DEP_INSTR `Op2 == ir_in3 `Dest)) || 
                ((`FRZ_DEP_INSTR `isReg) && (`FRZ_DEP_INSTR `Op2 == ir_in2 `Dest)) || 
                ((`FRZ_DEP_INSTR `isReg) && (`FRZ_DEP_INSTR `Op2 == ir_in0 `Dest)) ||  
                (`FRZ_DEP_INSTR `Dest == ir_in3 `Dest) ||
                (`FRZ_DEP_INSTR `Dest == ir_in2 `Dest) ||
                (`FRZ_DEP_INSTR `Dest == ir_in0 `Dest) ||
                (ir_in0 `CC == `S) || (ir_in2 `CC == `S) || (ir_in3 `CC == `S) ||
                ((`FRZ_DEP_INSTR == `OPldr) && ((ir_in0 `Opcode  == `OPstr) || (ir_in2 `Opcode  == `OPstr) || (ir_in3 `Opcode  == `OPstr))) ||
                ((`FRZ_DEP_INSTR == `OPstr) && ((ir_in0 `Opcode  == `OPldr) || (ir_in2 `Opcode  == `OPldr) || (ir_in3 `Opcode  == `OPldr)))) 
                begin
                ir_in2 <= `NOP;
                frz <= 1; 
            end else begin 
                
                frz <= 0; 

                if( !((`FRZ_DEP_INSTR `CC == `EQ && Zflag==0) || (`FRZ_DEP_INSTR `CC == `NE && Zflag==1))) begin
                    ir_in2 <= `FRZ_DEP_INSTR;
                    PC_in2 <= PC_in1;
                end

            end
        end
    end

/*
Stage 2
-"decode" the instruction (PRE,LDR,STR)
-sign extension
-read from register files
*/
    always @(posedge clk) begin
        if(fpu_done && !fpu_en) begin
            if(ir_in2[15:11] ==`OPpre) PREflag<=1;

            op1 <= regfile[ir_in2 `Dest];

                if (ir_in2 `isReg == 1) begin
                op2 <= regfile[ir_in2 `Op2];
                end else if (PREflag && (ir_in2!=`NOP)) begin
                op2 <= {PREval, ir_in2 `Op2};
                PREflag <=0;
                end else begin
                op2 <= {{12{ir_in2[3]}}, ir_in2 `Op2};
                end
            
            ir_in3 <= ir_in2;
            PC_in3 <= PC_in2;
        end
  end

/*
Stage 3
-Select Memory or ALU, perform appropriate operation
-Output the DestVal
*/
    always @(fpu_done && fpu_en) begin fpu_en <= 0; end

  always @(posedge clk) begin
    if(fpu_done && !fpu_en) begin
        //this case statement begins after we ensure we've made it to to the first instruction or else it halts prematurely
        if(ir_in3 === 16'bxxxxxxxxxxxxxxxx || ir_in3 === 16'b1xxxxxxxxxxxxxxx || ir_in3 [15:11] == `OPpre) begin #0; end 
        //the default needs to be halt instead of #1 so that it doesn't run infinitely
        else begin case (ir_in3 `Opcode) 
            `OPadd: begin outputVal<=op1+op2; regWrite<=1; end
            `OPsub: begin outputVal<=op1-op2; regWrite<=1; end
            `OPmul: begin outputVal<=op1*op2; regWrite<=1; end
            `OPand: begin outputVal<=op1&op2; regWrite<=1; end
            `OPorr: begin outputVal<=op1|op2; regWrite<=1; end
            `OPeor: begin outputVal<=op1^op2; regWrite<=1; end
            `OPbic: begin outputVal<=op1&~op2; regWrite<=1; end
            `OPslt: begin outputVal<=op1<op2; regWrite<=1; end
            `OPmov: begin outputVal<=op2; regWrite<=1; end
            `OPneg: begin outputVal<=-1*op2; regWrite<=1; end
            `OPsha: begin outputVal<=((op2>0) ? (op1 << op2) : (op1 >> -1*op2)); regWrite<=1; end
            `OPstr: begin datamem[op2] <= regfile[ir_in3 `Dest]; regWrite<=0; end
            `OPldr: begin outputVal<=datamem[op2]; regWrite<=1; end
            `OPitof: begin fpu_en <= 1; end //add FPU module instantiation
            default: begin halt<=1; end
        endcase
        end       

        //whenever the first instruction gets to this phase, init is set to 0... this may have to be moved elsewhere
        if(ir_in3 !== 16'bxxxxxxxxxxxxxxxx && init==1) init<=0;
            
        //just to make sure that we never enter an infinite loop
        if(ir_in3 === 16'bxxxxxxxxxxxxxxxx && init==0 && frz==0) begin
        halt<=1;
        end

        //to make sure that we have not entered an infinite loop... PC can never exceed 10000
        if(PC_in0==10000) begin
        halt<=1;
        end
        
        ir_in0 <= ir_in3;
    end
  end

endmodule

module testbench;
reg reset = 0;
reg clk = 0;
wire halted;
processor PE(halted,reset,clk);
initial begin
    $dumpfile("pfloat.vcp");                                
    //$dumpvars(0, PE );                    
    $dumpvars(0, PE, PE.regfile[0],PE.regfile[1],PE.regfile[2], PE.regfile[3]);      
    #10 reset = 1;
    #10 reset = 0;
    while (!halted) begin
        #10 clk = 1;
        #10 clk = 0;
    end
    $finish;
end
endmodule
