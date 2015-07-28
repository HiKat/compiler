	.text	
 main:
 	subu	$sp,$sp,32
 	sw	$ra,4($sp)
 	sw	$fp,0($sp)
 	addiu	$fp,$sp,4
 	li	$t0,99
 	sw	$t0,-4($fp)
 	li	$t0,99
 	sw	$t0,-8($fp)
 	li	$t0,99
 	sw	$t0,-12($fp)
 	li	$t0,99
 	sw	$t0,-16($fp)
 	li	$t0,99
 	sw	$t0,-20($fp)
 	li	$t0,100
 	sw	$t0,-24($fp)
 	lw	$t0,-20($fp)
 	sw	$t0,0($fp)
 	li	$v0,1
 	lw	$t0,0($fp)
 	move	$a0,$t0
 	syscall	
 	lw	$t0,-24($fp)
 	move	$v0,$t0
 	addiu	$sp,$sp,8
 	jr	$ra
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,32
 	jr	$ra
