	.data	
	.text	
cmp1:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,16
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,4($fp)
	lw	$t1,0($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	lw	$t1,8($fp)
	sgt	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
cmp2:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,16
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t1,0($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,4($fp)
	lw	$t1,-4($fp)
	sge	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,28
	jr	$ra
cmp3:
	subu	$sp,$sp,28
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,16
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t1,0($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,4($fp)
	lw	$t1,-4($fp)
	sgt	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
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
	subu	$sp,$sp,100
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,96
	li	$t0,2
	sw	$t0,-4($fp)
	li	$t0,3
	sw	$t0,-12($fp)
	lw	$t0,-4($fp)
	sw	$t0,-8($fp)
	lw	$t0,-12($fp)
	sw	$t0,-16($fp)
	lw	$t0,-8($fp)
	sw	$t0,-8($sp)
	lw	$t0,-16($fp)
	sw	$t0,-4($sp)
	jal	cmp1
	sw	$v0,0($fp)
	li	$t0,0
	sw	$t0,-20($fp)
	lw	$t0,0($fp)
	lw	$t1,-20($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-24($fp)
	li	$t0,4
	sw	$t0,-32($fp)
	li	$t0,3
	sw	$t0,-40($fp)
	lw	$t0,-32($fp)
	sw	$t0,-36($fp)
	lw	$t0,-40($fp)
	sw	$t0,-44($fp)
	lw	$t0,-36($fp)
	sw	$t0,-8($sp)
	lw	$t0,-44($fp)
	sw	$t0,-4($sp)
	jal	cmp2
	sw	$v0,-28($fp)
	li	$t0,1
	sw	$t0,-48($fp)
	lw	$t0,-28($fp)
	lw	$t1,-48($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-52($fp)
	lw	$t0,-24($fp)
	beqz	$t0,L18
	lw	$t0,-52($fp)
	beqz	$t0,L16
	li	$t0,1
	sw	$t0,-56($fp)
	j	L17
L16:
	li	$t0,0
	sw	$t0,-56($fp)
L17:
	j	L19
L18:
	li	$t0,0
	sw	$t0,-56($fp)
L19:
	li	$t0,4
	sw	$t0,-64($fp)
	li	$t0,3
	sw	$t0,-72($fp)
	lw	$t0,-64($fp)
	sw	$t0,-68($fp)
	lw	$t0,-72($fp)
	sw	$t0,-76($fp)
	lw	$t0,-68($fp)
	sw	$t0,-8($sp)
	lw	$t0,-76($fp)
	sw	$t0,-4($sp)
	jal	cmp3
	sw	$v0,-60($fp)
	li	$t0,0
	sw	$t0,-80($fp)
	lw	$t0,-60($fp)
	lw	$t1,-80($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-84($fp)
	lw	$t0,-56($fp)
	beqz	$t0,L22
	lw	$t0,-84($fp)
	beqz	$t0,L20
	li	$t0,1
	sw	$t0,-88($fp)
	j	L21
L20:
	li	$t0,0
	sw	$t0,-88($fp)
L21:
	j	L23
L22:
	li	$t0,0
	sw	$t0,-88($fp)
L23:
	li	$v0,1
	lw	$t0,-88($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,100
	jr	$ra