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
`define OPsys 5'b10011
`define OPpre 2'b11

//NOP instruction
`define NOP 16'b1xxxxxxxxxxxxxxx

//floating point unit
module fpu;
    input `WORD op1, `WORD op2, [5:0] instr;
    output `WORD result;
endmodule


module processor(halt, reset, clk);
    output reg halt;
    input reset, clk;

    reg `WORD regfile `REGSIZE;
    reg `WORD datamem `MEMSIZE;        //instantiate data memory
    reg `WORD instrmem `MEMSIZE;    //instantiate instruction memory
    reg init;
    reg frz;
    reg Zflag;
    reg PREflag; //tells whether the PRE is ready for use
    reg `PRESIZE PREval; //contains the PRE value, if there is one
    reg regWrite; //control mux

    always @(reset) begin
        halt = 0;
        init = 1;
        frz = 0;
        Zflag = 0;
        PC_in0=0;
        PREval=0;
        PREflag=0;
        $readmemh("instruction.mem", instrmem);          
        $readmemh("regfile.mem", regfile);               
        //$readmemh2(datamem);        
    end
    
        reg `WORD ir_in0, ir_in1, ir_in2, ir_in3, ir_inSaved;
        reg `WORD PC_in0, PC_in1, PC_in2, PC_in3;
        reg `WORD outputVal; //output from the ALU or MEM
        reg `WORD op1, op2;
       
   
/*
Stage 0 (owns PC)
-Determines the value of PC
-Sets the Z flag
-Performs writes to the regfile
*/
    always @(posedge clk) begin
      //writing to the register
      if ((!init) && regWrite && !(ir_in0 `Dest == 4'b1111)) regfile[ir_in0 `Dest] = outputVal;

      //setting the Z-flag
      if ((ir_in0 `CC == `S) && (ir_in0[15:14]!=2'b11) && (!init)) Zflag = !outputVal;

      if(ir_in0 `Dest == 4'b1111) begin //processing JUMPS
          PC_in0 <= outputVal+1;
          PC_in1 <= outputVal;
          regfile[15] <=outputVal;
          frz<=0;
      end else if(!frz) begin    //processing regular instructions that aren�t jumps
          PC_in0 <= PC_in0 + 1;
          PC_in1 <= PC_in0;
          if(PC_in3===16'bxxxxxxxxxxxxxxxx) begin
             regfile[15]<=0;    
          end else begin
             regfile[15] <= PC_in3+1; //setting the pc
          end
      end //end else if
      
    end

/*
Stage 1
-fetch an instruction from memory
-set PREval
-checks if the Zflag matches an EQ or NE instruction
*/

    always @(posedge clk) begin
  
      //These are the dependencies we check for. If there is a dependency, we send NOPs until it is resolved.
      if (instrmem[PC_in1-frz][15:14] == 2'b11) begin
          PREval<=instrmem[PC_in1-frz][11:0];
           frz <= 0; 
           if( !((instrmem[PC_in1-frz] `CC == `EQ && Zflag==0) || (instrmem[PC_in1-frz] `CC == `NE && Zflag==1))) begin
              ir_in2 <= instrmem[PC_in1-frz];
              PC_in2 <= PC_in1;
           end
      end else if ((ir_in3 `Dest == 4'b1111) ||
          (ir_in2 `Dest == 4'b1111) || 
          (ir_in0 `Dest == 4'b1111) || 
          ((instrmem[PC_in1-frz] `isReg) && (instrmem[PC_in1-frz] `Op2 == ir_in3 `Dest)) || 
          ((instrmem[PC_in1-frz] `isReg) && (instrmem[PC_in1-frz] `Op2 == ir_in2 `Dest)) || 
          ((instrmem[PC_in1-frz] `isReg) && (instrmem[PC_in1-frz] `Op2 == ir_in0 `Dest)) ||  
          (instrmem[PC_in1-frz] `Dest == ir_in3 `Dest) ||
          (instrmem[PC_in1-frz] `Dest == ir_in2 `Dest) ||
          (instrmem[PC_in1-frz] `Dest == ir_in0 `Dest) ||
          (ir_in0 `CC == `S) || (ir_in2 `CC == `S) || (ir_in3 `CC == `S) ||
          ((instrmem[PC_in1-frz] == `OPldr) && ((ir_in0 `Opcode  == `OPstr) || (ir_in2 `Opcode  == `OPstr) || (ir_in3 `Opcode  == `OPstr))) ||
          ((instrmem[PC_in1-frz] == `OPstr) && ((ir_in0 `Opcode  == `OPldr) || (ir_in2 `Opcode  == `OPldr) || (ir_in3 `Opcode  == `OPldr)))) begin
          ir_in2 <= `NOP;
          frz <= 1; 
      end else begin 
         
          frz <= 0; 

          if( !((instrmem[PC_in1-frz] `CC == `EQ && Zflag==0) || (instrmem[PC_in1-frz] `CC == `NE && Zflag==1))) begin
              ir_in2 <= instrmem[PC_in1-frz];
              PC_in2 <= PC_in1;
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

       if(ir_in2[15:14] ==`OPpre) PREflag<=1;

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

/*
Stage 3
-Select Memory or ALU, perform appropriate operation
-Output the DestVal
*/
  always @(posedge clk) begin
    //this case statement begins after we ensure we've made it to to the first instruction or else it halts prematurely
    if(ir_in3 === 16'bxxxxxxxxxxxxxxxx || ir_in3 === 16'b1xxxxxxxxxxxxxxx || ir_in3 [15:14] == `OPpre) begin #0; end 
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