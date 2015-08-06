	.data	
	.text	
logi1:
	subu	$sp,$sp,64
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,44
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t1,0($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,4($fp)
	lw	$t1,-4($fp)
	slt	$t0,$t0,$t1
	sw	$t0,-8($fp)
	li	$t0,1
	sw	$t0,-12($fp)
	lw	$t0,8($fp)
	lw	$t1,-12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-16($fp)
	lw	$t0,-16($fp)
	lw	$t1,12($fp)
	slt	$t0,$t0,$t1
	sw	$t0,-20($fp)
	lw	$t0,-8($fp)
	beqz	$t0,L36
	lw	$t0,-20($fp)
	beqz	$t0,L34
	li	$t0,1
	sw	$t0,-24($fp)
	j	L35
L34:
	li	$t0,0
	sw	$t0,-24($fp)
L35:
	j	L37
L36:
	li	$t0,0
	sw	$t0,-24($fp)
L37:
	li	$t0,0
	sw	$t0,-28($fp)
	lw	$t0,8($fp)
	lw	$t1,-28($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-32($fp)
	lw	$t0,-24($fp)
	beqz	$t0,L40
	li	$t0,1
	sw	$t0,-36($fp)
	j	L41
L40:
	lw	$t0,-32($fp)
	beqz	$t0,L38
	li	$t0,1
	sw	$t0,-36($fp)
	j	L39
L38:
	li	$t0,0
	sw	$t0,-36($fp)
L39:
L41:
	lw	$t0,-36($fp)
	beqz	$t0,L42
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,64
	jr	$ra
	j	L43
L42:
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,64
	jr	$ra
L43:
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,64
	jr	$ra
logi2:
	subu	$sp,$sp,40
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,20
	lw	$t0,4($fp)
	lw	$t1,8($fp)
	slt	$t0,$t0,$t1
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t1,12($fp)
	slt	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,0($fp)
	beqz	$t0,L46
	lw	$t0,-4($fp)
	beqz	$t0,L44
	li	$t0,1
	sw	$t0,-8($fp)
	j	L45
L44:
	li	$t0,0
	sw	$t0,-8($fp)
L45:
	j	L47
L46:
	li	$t0,0
	sw	$t0,-8($fp)
L47:
	lw	$t0,-8($fp)
	beqz	$t0,L50
	li	$t0,1
	sw	$t0,-12($fp)
	j	L51
L50:
	lw	$t0,16($fp)
	beqz	$t0,L48
	li	$t0,1
	sw	$t0,-12($fp)
	j	L49
L48:
	li	$t0,0
	sw	$t0,-12($fp)
L49:
L51:
	lw	$t0,-12($fp)
	beqz	$t0,L52
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
	j	L53
L52:
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
L53:
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
logi3:
	subu	$sp,$sp,40
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,28
	li	$t0,3
	sw	$t0,0($fp)
	lw	$t0,8($fp)
	lw	$t1,0($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,8($fp)
	lw	$t1,4($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,-4($fp)
	beqz	$t0,L56
	lw	$t0,-8($fp)
	beqz	$t0,L54
	li	$t0,1
	sw	$t0,-12($fp)
	j	L55
L54:
	li	$t0,0
	sw	$t0,-12($fp)
L55:
	j	L57
L56:
	li	$t0,0
	sw	$t0,-12($fp)
L57:
	lw	$t0,-12($fp)
	beqz	$t0,L64
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,4($fp)
	lw	$t1,0($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,4($fp)
	lw	$t1,8($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,4($fp)
	lw	$t1,8($fp)
	mul	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-8($fp)
	lw	$t1,-12($fp)
	slt	$t0,$t0,$t1
	sw	$t0,-16($fp)
	lw	$t0,-4($fp)
	beqz	$t0,L60
	li	$t0,1
	sw	$t0,-20($fp)
	j	L61
L60:
	lw	$t0,-16($fp)
	beqz	$t0,L58
	li	$t0,1
	sw	$t0,-20($fp)
	j	L59
L58:
	li	$t0,0
	sw	$t0,-20($fp)
L59:
L61:
	lw	$t0,-20($fp)
	beqz	$t0,L62
	li	$t0,1
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
	j	L63
L62:
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
L63:
	j	L65
L64:
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
L65:
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,40
	jr	$ra
main:
	subu	$sp,$sp,108
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,104
	li	$t0,1
	sw	$t0,-4($fp)
	li	$t0,2
	sw	$t0,-12($fp)
	li	$t0,4
	sw	$t0,-20($fp)
	li	$t0,3
	sw	$t0,-28($fp)
	lw	$t0,-4($fp)
	sw	$t0,-8($fp)
	lw	$t0,-12($fp)
	sw	$t0,-16($fp)
	lw	$t0,-20($fp)
	sw	$t0,-24($fp)
	lw	$t0,-28($fp)
	sw	$t0,-32($fp)
	lw	$t0,-8($fp)
	sw	$t0,-16($sp)
	lw	$t0,-16($fp)
	sw	$t0,-12($sp)
	lw	$t0,-24($fp)
	sw	$t0,-8($sp)
	lw	$t0,-32($fp)
	sw	$t0,-4($sp)
	jal	logi1
	sw	$v0,0($fp)
	li	$t0,1
	sw	$t0,-40($fp)
	li	$t0,2
	sw	$t0,-48($fp)
	li	$t0,3
	sw	$t0,-56($fp)
	li	$t0,0
	sw	$t0,-64($fp)
	lw	$t0,-40($fp)
	sw	$t0,-44($fp)
	lw	$t0,-48($fp)
	sw	$t0,-52($fp)
	lw	$t0,-56($fp)
	sw	$t0,-60($fp)
	lw	$t0,-64($fp)
	sw	$t0,-68($fp)
	lw	$t0,-44($fp)
	sw	$t0,-16($sp)
	lw	$t0,-52($fp)
	sw	$t0,-12($sp)
	lw	$t0,-60($fp)
	sw	$t0,-8($sp)
	lw	$t0,-68($fp)
	sw	$t0,-4($sp)
	jal	logi2
	sw	$v0,-36($fp)
	lw	$t0,0($fp)
	beqz	$t0,L68
	lw	$t0,-36($fp)
	beqz	$t0,L66
	li	$t0,1
	sw	$t0,-72($fp)
	j	L67
L66:
	li	$t0,0
	sw	$t0,-72($fp)
L67:
	j	L69
L68:
	li	$t0,0
	sw	$t0,-72($fp)
L69:
	li	$t0,2
	sw	$t0,-80($fp)
	li	$t0,3
	sw	$t0,-88($fp)
	lw	$t0,-80($fp)
	sw	$t0,-84($fp)
	lw	$t0,-88($fp)
	sw	$t0,-92($fp)
	lw	$t0,-84($fp)
	sw	$t0,-8($sp)
	lw	$t0,-92($fp)
	sw	$t0,-4($sp)
	jal	logi3
	sw	$v0,-76($fp)
	lw	$t0,-72($fp)
	beqz	$t0,L72
	lw	$t0,-76($fp)
	beqz	$t0,L70
	li	$t0,1
	sw	$t0,-96($fp)
	j	L71
L70:
	li	$t0,0
	sw	$t0,-96($fp)
L71:
	j	L73
L72:
	li	$t0,0
	sw	$t0,-96($fp)
L73:
	li	$v0,1
	lw	$t0,-96($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,108
	jr	$ra