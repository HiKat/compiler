	.data	
	.text	
s:
	subu	$sp,$sp,32
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,28
	li	$t0,1
	sw	$t0,0($fp)
	li	$t0,2
	sw	$t0,-4($fp)
	li	$t0,3
	sw	$t0,-12($fp)
	lw	$t0,-4($fp)
	lw	$t1,-12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	sw	$t0,-4($fp)
	lw	$t0,0($fp)
	lw	$t1,-4($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	sw	$t0,0($fp)
	li	$t0,4
	sw	$t0,-16($fp)
	lw	$t0,0($fp)
	lw	$t1,-16($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,32
	jr	$ra
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,32
	jr	$ra
main:
	subu	$sp,$sp,20
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,16
	jal	s
	sw	$v0,0($fp)
	li	$t0,10
	sw	$t0,-4($fp)
	lw	$t0,0($fp)
	lw	$t1,-4($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-8($fp)
	li	$v0,1
	lw	$t0,-8($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,20
	jr	$ra