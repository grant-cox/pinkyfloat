// basic sizes of things
`define DATA	[15:0]
`define ADDR	[15:0]
`define SIZE	[65535:0]
`define INST	[15:0]
`define WORD [15:0]
`define CC	[15:14]
`define OP	[14:9]
`define IORR	[8]
`define RD	[7:4]
`define RN	[3:0]
`define REGS    [15:0]

// CC values
`define AL	0
`define S	1
`define NE	2
`define EQ	3

// opcode values, also state numbers
`define OPPRE		5'h00
`define OPADD		5'h08
`define OPAND		5'h09
`define OPBIC		5'h0a
`define OPEOR		5'h0b
`define OPMUL		5'h0c
`define OPORR		5'h0d
`define OPSHA		5'h0e
`define OPSLT		5'h0f
`define OPSUB		5'h10
`define OPADDF		5'h11
`define OPFTOI		5'h12
`define OPITOF		5'h13
`define OPMULF		5'h14
`define OPRECF		5'h15
`define OPSUBF		5'h16
`define OPMOV		5'h17
`define OPNEG		5'h18
`define OPLDR		5'h19
`define OPSTR		5'h1a
`define OPSYS		5'h1f

// make NOP (after fetch) an unconditional PRE 0
`define NOP             16'b0 

//Floating point definition (syntax)
`define BIAS 8'b10000110 //exponent exp = E - bias - 7, so total BIAS = 134
`define SIGN [15] //1 bit leading sign
`define EXP [14:7] //8 bit exponent
`define MANT [6:0] //7 bit mantissa (implied leading 1.mantissa)


module srl(dst, src, shift);
    output reg[7:0] dst; input wire[7:0] src, shift;
    reg[7:0] by1, by2, by4;
    always @(*) begin
        by1 = (shift[0] ? {1'b0, src[7:1]} : src);
        by2 = (shift[1] ? {2'b0, by1[7:2]} : by1);
        by4 = (shift[2] ? {4'b0, by2[7:4]} : by2);
        dst = (shift[7:3] ? 0 : by4);
    end
endmodule

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


`define FPU_START 50
`define FPU_ITOF_S2 51
`define FPU_ITOF_S3 52
`define FPU_FTOI_S2 53
`define FPU_MULF_S2 54
`define FPU_RECF_S2 55
`define FPU_ADDF_S2 56
`define FPU_ADDF_S3 57
`define FPU_ADDF_S4 58
`define FPU_ADDF_S5 59

//floating point unit
//reg done : 1 when the floating point operation has completed
//reg done : 0 when the floating point operation is currently running
module fpu(input en, input clk, input `WORD op1, input `WORD op2, input [4:0] instr, output reg `WORD result, output reg done);
    initial begin
        done <= 0;
    end


    integer state = `FPU_START;
    integer tmp;
    reg sign;
    reg [7:0] exp;
    reg [7:0] exp_p1; //for MULF instruction, this is the speculative exponent result if the mantissa multiply overflows
    reg `MANT frac;
    reg [7:0] frac_w1;
    reg [7:0] lrg_frac_w1;
    reg overflow; //tracks overflow of the mantissa addition for ADDF
    reg `WORD larger; //larger of the two operands for ADDF and SUBF
    reg `WORD smaller; //smaller of the two operands for ADDF and SUBF
    reg [7:0] shift; //difference between the two exponent values for ADDF/SUBF
    reg `WORD int;
    //reg `WORD mant_mul;
    wire [4:0] d; //output of the count leading 0s module
    wire [4:0] d_tmp;
    wire [7:0] srl_out; //output of barrel shifter module

    lead0s lead0(.d(d), .s(int));
    lead0s lead1(.d(d_tmp), .s(tmp[15:0]));
    srl mysrl(.dst(srl_out), .src( {1'b1, smaller[6:0]} ), .shift(shift)); //module srl(dst, src, shift);

    reg [7:0] reciprocal_lookup [127:0];
    initial begin
        $readmemh("reciprocal_look.mem", reciprocal_lookup);
    end

    always @(posedge clk) begin
        if (en) begin
 	            case(state)
                `FPU_START: begin
					done <= 0; //operation is currently running
					//$display("instr: %b `OPITOF %b op2 %b ", instr, `OPITOF, op2);
                    case (instr)
                        `OPITOF: begin
							//$display("start op2 %d", op2);
                            if(op2) begin           //if op2 ain't 0 
                                sign <= op2[15];
                                if(op2[15]) int <= (~op2) + 1; //possible error: may not execute in 1 clock cycle (2's compliment)
                                else int <= op2;
                                state = `FPU_ITOF_S2;
                            end else begin          //if we's a dummy and try to convert 0 to float 
                                result <= 0;
                                done <= 1;
                            end
                        end
                        `OPFTOI: begin 
                            if(op2) begin
                                if(op2 `EXP > 134) //neg case
                                    result <= {1'b1, op2 `MANT} << (op2 `EXP - 134);
                                else
                                    result <= {1'b1, op2 `MANT} >> (134 - op2 `EXP); //working for pos only
                                
                                if (op2 `SIGN) state <= `FPU_FTOI_S2;
                                else done <= 1;
                            end else begin
                                result <= 0;
                                done <= 1;
                            end
                        end
                        `OPMULF: begin
                            //if either operands are 0, then the result will be 0
                            if( (op1 == 0) || (op2 == 0) ) begin
                                result <= 0;
                                done <= 1;
                            end else begin
                                sign <= (op1 `SIGN ^ op2 `SIGN); 
                                exp <= ((op1 `EXP + op2 `EXP) - 127); //add exponents and subtract bias for result exp
                                exp_p1 <= ((op1 `EXP + op2 `EXP) - 126);
                                int <= ({1'b1, op1 `MANT} * {1'b1,op2 `MANT}); //multiply the mantissa and store into 16 bit container
                                state <= `FPU_MULF_S2;
                            end

                        end
                        `OPRECF: begin 
                            sign <= op2 `SIGN;
                            exp <= 253 - op2 `EXP;
                            frac <= reciprocal_lookup[op2 `MANT];
                            state <= `FPU_RECF_S2;
                        end
                        `OPSUBF: begin end
                        `OPADDF: begin
                            if((op1 `SIGN == op2 `SIGN)) begin //pos pos and neg neg
                                sign <= op1 `SIGN; //result will be positive
                                state <= `FPU_ADDF_S3;
                            end
                            else begin //opposite signs
                                state <= `FPU_ADDF_S2; //go to two's compliment stage 
                                if(op1 `EXP > op2 `EXP)         sign <= op1 `SIGN;
                                else                            sign <= op2 `SIGN;
                            end

                            if(op1 `EXP > op2 `EXP) begin //op2 exponent will be adjusted to match op1
                                shift <= op1 `EXP - op2 `EXP; //determine the difference between the two exponents
                                larger <= op1;
                                smaller <= op2;
                                exp <= op1 `EXP;
                            end else if(op1 `EXP < op2 `EXP) begin //op1 exponent will be adjusted to match op2
                                shift <= op2 `EXP - op1 `EXP; //determine the difference between the two exponents
                                larger <= op2;
                                smaller <= op1;
                                exp <= op2 `EXP;
                            end else begin
                                shift <= 0;
                                larger <= op1;
                                smaller <= op2;
                                exp <= op1 `EXP;
                            end
                        end
                        default: begin end
                    endcase 
                end
                
                `FPU_ITOF_S2: begin
                    exp <= 127 + (15 - d);
                    tmp <= (int << (d + 1));
                    state = `FPU_ITOF_S3;
                end
                `FPU_ITOF_S3: begin 
                    result <= {sign, exp, tmp[15:9]};
                    done <= 1; 
					state = `FPU_START;
                end

                `FPU_FTOI_S2: begin
                    result <= ~result + 1;
                    done <= 1;
                    state = `FPU_START;
                end

                `FPU_MULF_S2: begin
                    if(int[15] == 1) begin
                        result <= {sign, exp_p1, int[14:8]};
                    end else begin
                        result <= {sign, exp, {int[13:8], 1'b0}};
                    end
                    done <= 1;
                end

                `FPU_RECF_S2: begin
                    result <= {sign, exp, frac};
                    done <= 1;
                end

                `FPU_ADDF_S2: begin //subtraction
                    //perform twos complement operation
                    tmp <= { 1'b1, larger `MANT } - srl_out ; //subtract the bastards
                    state <= `FPU_ADDF_S4;
                end

                `FPU_ADDF_S3: begin //addition
                    int <= srl_out + {1'b1, larger[6:0]};
                    state <= `FPU_ADDF_S4;
                    #1 $display("FPU_ADDF_S3: int = %b + %b = %b", srl_out, {1'b1, larger[6:0]}, int);
                end

                `FPU_ADDF_S4: begin
                    //int <= int << (d + 1); //16 bit leading zero counter -- we are passing 8 bits to it, remove useless top 7
                    if(int[8]) begin        // mantissa overflow case
                        $display("mantissa overflow.");
                        frac <= int[7:1];
                        exp <= exp + 1;
                    end
                    else if(int[7]) begin //non overflow case
                        $display("no mantissa overflow.");
                        frac <= int[6:0];
                    end
                    
                    if(d_tmp) begin // if there are leading zeros on tmp after subtraction
                        frac <= tmp << d_tmp-8; //there are 8 leading 0's gauranteed on tmp\
                        exp <= exp - d_tmp-8;
                    end
                    state <= `FPU_ADDF_S5;
                end

                `FPU_ADDF_S5: begin
                    result <= {sign, exp, frac};
                    done <= 1;
                end



            endcase
        end
    end
endmodule

module testbench;
    reg clk = 0;
    integer counter = 0;
    wire `DATA result;
    wire done;
    reg [4:0] instr = `OPADDF;
    reg `DATA rd = 16'hc179;
    reg `DATA rn = 16'h41a3; 
    reg en;
    fpu myfpu(.en(en), .clk(clk), .op1(rd), .op2(rn), .instr(instr), .result(result), .done(done));

    initial begin
        $dumpfile("pfloat.vcp"); 
        //$dumpvars(0, PE, PE.r[0],PE.r[1],PE.r[2], PE.r[3]);
        $dumpvars();
        #4 en = 1;

        while (counter < 5) begin
            #5 clk = 1;
            #5 clk = 0;
            counter = counter + 1;
        end
        $finish;
    end

endmodule