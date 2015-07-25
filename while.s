	.text	
 w:
 	subu	$sp,$sp,72
 	sw	$ra,4($sp)
 	sw	$fp,0($sp)
 	addiu	$fp,$sp,64
 	li	$t0,0
 	sw	$t0,-56($fp)
 	lw	$t0,4($fp)
 	sw	$t0,-40($fp)
 	li	$t0,0
 	sw	$t0,-36($fp)
 	lw	$t0,-40($fp)
 	lw	$t1,-36($fp)
 	bgt	$t0,$t1,L0
 	li	$t0,0
 	sw	$t0,-44($fp)
 	j	L1
 L0:
 	li	$t0,1
 	sw	$t0,-44($fp)
 L1:
 	lw	$t0,-44($fp)
 L2:
 	beqz	$t0,L3
 	lw	$t0,-56($fp)
 	sw	$t0,-24($fp)
 	lw	$t0,4($fp)
 	sw	$t0,-20($fp)
 	lw	$t0,-24($fp)
 	lw	$t1,-20($fp)
 	add	$t0,$t0,$t1
 	sw	$t0,-56($fp)
 	lw	$t0,4($fp)
 	sw	$t0,-8($fp)
 	li	$t0,1
 	sw	$t0,-4($fp)
 	lw	$t0,-8($fp)
 	lw	$t1,-4($fp)
 	sub	$t0,$t0,$t1
 	sw	$t0,4($fp)
 	j	L2
 L3:
 	lw	$t0,-56($fp)
 	sw	$t0,0($fp)
 	lw	$v0,0($fp)
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,72
 	jr	$ra
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,72
 	jr	$ra

	.data	
 
	.text	
 
	.globl	main
 main:
 	subu	$sp,$sp,56
 	sw	$ra,4($sp)
 	sw	$fp,0($sp)
 	addiu	$fp,$sp,52
 	li	$t0,10
 	sw	$t0,-4($fp)
 	lw	$t0,-4($fp)
 	sw	$t0,-4($sp)
 	jal	w
 	sw	$v0,-12($fp)
 	li	$t0,55
 	sw	$t0,-8($fp)
 	lw	$t0,-12($fp)
 	lw	$t1,-8($fp)
 	beq	$t0,$t1,L4
 	li	$t0,0
 	sw	$t0,-20($fp)
 	j	L5
 L4:
 	li	$t0,1
 	sw	$t0,-20($fp)
 L5:
 	li	$v0,1
 	lw	$t0,-20($fp)
 	move	$a0,$t0
 	syscall	
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,56
 	jr	$ra