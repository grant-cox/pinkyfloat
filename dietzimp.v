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


`define FPU_START 50
`define FPU_ITOF_S2 51
`define FPU_ITOF_S3 52

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
                        `OPITOF: begin
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
                        `OPFTOI: begin end
                        `OPMULF: begin end
                        `OPRECF: begin end
                        `OPSUBF: begin end
                        `OPADDF: begin end
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

    reg `DATA r `REGS;	// register file
    reg `DATA d `SIZE;	// data memory
    reg `INST i `SIZE;	// instruction memory
    reg [7:0] reciprocal_lookup [127:0];
    reg `ADDR pc;		// program counter
    reg `ADDR tpc, pc0, pc1;
    reg `INST ir;		// instruction register
    reg `INST ir0, ir1;
    reg `DATA im0, rd1, rn1, res;
    reg `ADDR target;	// jump target
    reg jump;		// are we jumping?
    reg zreg;		// z flag
    wire pendz;		// z update pending?
    wire pendpc;		// pc update pending?
    reg wait1;		// need to stall in stage 1?
    reg [11:0] prefix;	// 12-bit prefix value
    reg havepre;		// is prefix valid?
	
	
	wire `DATA fpu_result;
	wire fpu_done = 1;
	reg fpu_en = 0;
	fpu myfpu(.en(fpu_en), .clk(clk), .op1(rd1), .op2(rn1), .instr(ir1 [13:9]), .result(fpu_result), .done(fpu_done));
	
	wire systemhalt; assign systemhalt = fpu_en && !fpu_done;
	//escape clause for nasty ifs
	// always @ (negedge clk && fpu_en && fpu_done) begin //when the fpu is finished, set result and continue
	// 	fpu_en <= 0;
	// 	res <= fpu_result;
	// end


    always @(reset) begin
      halt = 0;
      pc = 0;
      ir0 = `NOP;
      ir1 = `NOP;
      jump = 0;
      havepre = 0;
	  //fpu_done = 1;
	  fpu_en = 0;

    // use the following with dollars to initialize
      $readmemh("instruction.mem", i);          
      $readmemh("regfile.mem", r);                      
      $readmemh("reciprocal_look.mem", reciprocal_lookup);
    end

    //does the instruction set Rd?
    function setsrd;
    input `INST inst;
    setsrd = ((inst `OP >= `OPADD) && (inst `OP < `OPSTR));
    endfunction

    //does the instruction set PC?
    function setspc;
    input `INST inst;
    setspc = ((inst `RD == 15) && setsrd(inst));
    endfunction

    //does the instruction set z flag?
    function setsz;
    input `INST inst;
    setsz = ((inst `CC == `S) && setsrd(inst));
    endfunction

    //is the instruction conditional?
    function iscond;
    input `INST inst;
    iscond = ((inst `CC == `NE) || (inst `CC == `EQ));
    endfunction

    //does the instruction use an immediate?
    function usesim;
    input `INST inst;
    usesim = ((inst `IORR) && (inst `OP <= `OPSTR));
    endfunction

    //function operation stores into a register
    function usesrd;
    input `INST inst;
    usesrd = ((inst `OP == `OPADD) ||
              (inst `OP == `OPADDF) ||
              (inst `OP == `OPAND) ||
              (inst `OP == `OPBIC) ||
              (inst `OP == `OPEOR) ||
              (inst `OP == `OPMUL) ||
              (inst `OP == `OPMULF) ||
              (inst `OP == `OPORR) ||
              (inst `OP == `OPSHA) ||
              (inst `OP == `OPSTR) ||
              (inst `OP == `OPSLT) ||
              (inst `OP == `OPSUB) ||
              (inst `OP == `OPSUBF) ||
			  (inst `OP == `OPITOF) ||
			  (inst `OP ==  `OPFTOI));
    endfunction

    //IORR: 1 = immed, 0 = register
    function usesrn;
    input `INST inst;
    usesrn = ((!(inst `IORR)) && (inst `OP <= `OPSTR));
    endfunction

    // pending z update?
    assign pendz = (setsz(ir0) || setsz(ir1));

    // pending PC update?
    assign pendpc = (setspc(ir0) || setspc(ir1));

    // stage 0: instruction fetch and immediate extend
    always @(posedge clk) begin
		if(!systemhalt) begin
			tpc = (jump ? target : pc);

			if (wait1) begin
				// blocked by stage 1, so should not have a jump, but...
				pc <= tpc;
			end else begin
				// not blocked by stage 1
				ir = i[tpc];

				if (pendpc || (iscond(ir) && pendz)) begin
				// waiting... pc doesn't change
				ir0 <= `NOP;
				pc <= tpc;
				end else begin
				if (ir[13:12] == 0) begin
					// PRE operation
					havepre <= 1;
					prefix <= ir[11:0];
					ir0 <= `NOP; //send NOP to next stage
				end else begin
					if (usesim(ir)) begin
					// extend immediate
					im0 <= {(havepre ? prefix : {12{ir[3]}}), ir `RN};
					havepre <= 0;
					end
					ir0 <= ir;
				end
				pc <= tpc + 1;
				end

				pc0 <= tpc;
			end
		end
    end

    // stage 1: register read
    always @(posedge clk) begin
		if(!systemhalt) begin
			if ((ir0 != `NOP) &&
				setsrd(ir1) &&
				((usesrd(ir0) && (ir0 `RD == ir1 `RD)) ||
				(usesrn(ir0) && (ir0 `RN == ir1 `RD)))) begin
				// stall waiting for register value
				wait1 = 1;
				ir1 <= `NOP;
			end else begin
				// all good, get operands (even if not needed)
				wait1 = 0;
				rd1 <= ((ir0 `RD == 15) ? pc0 : r[ir0 `RD]);
				rn1 <= (usesim(ir0) ? im0 :
						((ir0 `RN == 15) ? pc0 : r[ir0 `RN]));
				ir1 <= ir0;
			end
		end
    end

    // stage 2: ALU, data memory access, store in register
    always @(posedge clk) begin
		$display("ir1 `OP %b fpu_en %b fpu_done %b", ir1 `OP, fpu_en ,fpu_done); #1
		if(!systemhalt) begin
			if ((ir1 == `NOP) ||
				((ir1 `CC == `EQ) && (zreg == 0)) ||
				((ir1 `CC == `NE) && (zreg == 1))) begin
				// condition says nothing happens
				$display("evil statment");
				jump <= 0;
			end else begin
				$display("case");
				// let the instruction execute
				case (ir1 `OP)
				`OPPRE:  begin end // do nothing
				`OPADD:  res = rd1 + rn1;
				`OPAND:  res = rd1 & rn1;
				`OPBIC:  res = rd1 & ~rn1;
				`OPEOR:  res = rd1 ^ rn1;
				`OPMUL:  res = rd1 * rn1;
				`OPORR:  res = rd1 | rn1;
				`OPSHA:  res = ((rn1 > 0) ? (rd1 << rn1) : (rd1 >> -rn1));
				`OPSLT:  res = (rd1 < rn1);
				`OPSUB:  res = rd1 - rn1;
				`OPMOV:  res = rn1;
				`OPNEG:  res = -rn1;
				`OPLDR:  res = d[rn1];
				`OPSTR:  begin res = rd1; d[rn1] <= res; end
				`OPITOF: begin fpu_en <= 1; #1 $display("testing"); end
				default: halt <= 1; // make it stop
				endcase

				// update z flag if we should
				if (setsz(ir1)) zreg <= (res == 0);

				// put result in rd if we should
				if (setsrd(ir1)) begin
				if (ir1 `RD == 15) begin
					jump <= 1;
					target <= res;
				end else begin
					r[ir1 `RD] <= res;
					jump <= 0;
				end
				end else jump <= 0;
			end
		end
	end
endmodule

module testbench;
    reg reset = 0;
    reg clk = 0;
    wire halted;
    processor PE(halted, reset, clk);
    initial begin
      $dumpfile("pfloat.vcp"); 
      $dumpvars(0, PE, PE.r[0],PE.r[1],PE.r[2], PE.r[3]);
      #10 reset = 1;
      #10 reset = 0;
      while (!halted) begin
        #10 clk = 1;
        #10 clk = 0;
      end
      $finish;
    end
endmodule


