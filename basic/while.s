	.data	
	.text	
w:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,20
	li	$t0,0
	sw	$t0,0($fp)
	li	$t0,0
	sw	$t0,-4($fp)
	lw	$t0,4($fp)
	lw	$t1,-4($fp)
	sgt	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
L78:
	beqz	$t0,L79
	lw	$t0,0($fp)
	lw	$t1,4($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	sw	$t0,0($fp)
	li	$t0,1
	sw	$t0,-8($fp)
	lw	$t0,4($fp)
	lw	$t1,-8($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-12($fp)
	sw	$t0,4($fp)
	j	L78
L79:
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
main:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,24
	li	$t0,10
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	sw	$t0,-4($sp)
	jal	w
	sw	$v0,0($fp)
	li	$t0,55
	sw	$t0,-12($fp)
	lw	$t0,0($fp)
	lw	$t1,-12($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-16($fp)
	li	$v0,1
	lw	$t0,-16($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra