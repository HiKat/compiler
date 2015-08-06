	.data	
	.text	
swap:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,16
	lw	$t0,4($fp)
	lw	$t0,0($t0)
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t0,0($t0)
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	lw	$t1,4($fp)
	sw	$t0,0($t1)
	lw	$t0,0($fp)
	lw	$t1,8($fp)
	sw	$t0,0($t1)
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
main:
	subu	$sp,$sp,48
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,44
	li	$t0,1
	sw	$t0,0($fp)
	li	$t0,2
	sw	$t0,-4($fp)
	la	$t0,0($fp)
	sw	$t0,-12($fp)
	la	$t0,-4($fp)
	sw	$t0,-16($fp)
	lw	$t0,-12($fp)
	sw	$t0,-8($sp)
	lw	$t0,-16($fp)
	sw	$t0,-4($sp)
	jal	swap
	sw	$v0,-8($fp)
	li	$t0,2
	sw	$t0,-20($fp)
	lw	$t0,0($fp)
	lw	$t1,-20($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-24($fp)
	li	$t0,1
	sw	$t0,-28($fp)
	lw	$t0,-4($fp)
	lw	$t1,-28($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-32($fp)
	lw	$t0,-24($fp)
	beqz	$t0,L76
	lw	$t0,-32($fp)
	beqz	$t0,L74
	li	$t0,1
	sw	$t0,-36($fp)
	j	L75
L74:
	li	$t0,0
	sw	$t0,-36($fp)
L75:
	j	L77
L76:
	li	$t0,0
	sw	$t0,-36($fp)
L77:
	li	$v0,1
	lw	$t0,-36($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,48
	jr	$ra