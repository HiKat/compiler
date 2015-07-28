	.text	
 main:
 	subu	$sp,$sp,8
 	sw	$ra,4($sp)
 	sw	$fp,0($sp)
 	addiu	$fp,$sp,4
 	li	$t0,8
 	sw	$t0,-4($fp)
 	li	$v0,1
 	lw	$t0,-4($fp)
 	move	$a0,$t0
 	syscall	
 	lw	$t0,0($fp)
 	move	$v0,$t0
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,8
 	jr	$ra
 	lw	$fp,0($sp)
 	lw	$ra,4($sp)
 	addiu	$sp,$sp,8
 	jr	$ra
