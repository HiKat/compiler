	.data	
n:	.word 0
	.text	
get:
	subu	$sp,$sp,44
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,28
	lw	$t0,8($fp)
	lw	$t1,n
	mul	$t0,$t0,$t1
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	lw	$t1,12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	li	$t0,4
	sw	$t0,-16($fp)
	lw	$t0,-8($fp)
	lw	$t1,-16($fp)
	mul	$t0,$t0,$t1
	sw	$t0,-20($fp)
	lw	$t0,4($fp)
	lw	$t1,-20($fp)
	add	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-12($fp)
	lw	$t0,0($t0)
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	move	$v0,$t0
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,44
	jr	$ra
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,44
	jr	$ra
set:
	subu	$sp,$sp,44
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,24
	lw	$t0,8($fp)
	lw	$t1,n
	mul	$t0,$t0,$t1
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	lw	$t1,12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-4($fp)
	li	$t0,4
	sw	$t0,-12($fp)
	lw	$t0,-4($fp)
	lw	$t1,-12($fp)
	mul	$t0,$t0,$t1
	sw	$t0,-16($fp)
	lw	$t0,4($fp)
	lw	$t1,-16($fp)
	add	$t0,$t0,$t1
	sw	$t0,-8($fp)
	lw	$t0,16($fp)
	lw	$t1,-8($fp)
	sw	$t0,0($t1)
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,44
	jr	$ra
matmul:
	subu	$sp,$sp,84
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,68
	li	$t0,0
	sw	$t0,0($fp)
	lw	$t0,0($fp)
	lw	$t1,n
	slt	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-12($fp)
L48:
	beqz	$t0,L49
	li	$t0,0
	sw	$t0,-4($fp)
	lw	$t0,-4($fp)
	lw	$t1,n
	slt	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-12($fp)
L46:
	beqz	$t0,L47
	li	$t0,0
	sw	$t0,-20($fp)
	li	$t0,0
	sw	$t0,-8($fp)
	lw	$t0,-8($fp)
	lw	$t1,n
	slt	$t0,$t0,$t1
	sw	$t0,-12($fp)
	lw	$t0,-12($fp)
L44:
	beqz	$t0,L45
	lw	$t0,4($fp)
	sw	$t0,-16($fp)
	lw	$t0,0($fp)
	sw	$t0,-24($fp)
	lw	$t0,-8($fp)
	sw	$t0,-28($fp)
	lw	$t0,-16($fp)
	sw	$t0,-12($sp)
	lw	$t0,-24($fp)
	sw	$t0,-8($sp)
	lw	$t0,-28($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-12($fp)
	lw	$t0,8($fp)
	sw	$t0,-36($fp)
	lw	$t0,-8($fp)
	sw	$t0,-40($fp)
	lw	$t0,-4($fp)
	sw	$t0,-44($fp)
	lw	$t0,-36($fp)
	sw	$t0,-12($sp)
	lw	$t0,-40($fp)
	sw	$t0,-8($sp)
	lw	$t0,-44($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-32($fp)
	lw	$t0,-12($fp)
	lw	$t1,-32($fp)
	mul	$t0,$t0,$t1
	sw	$t0,-48($fp)
	lw	$t0,-20($fp)
	lw	$t1,-48($fp)
	add	$t0,$t0,$t1
	sw	$t0,-52($fp)
	lw	$t0,-52($fp)
	sw	$t0,-20($fp)
	li	$t0,1
	sw	$t0,-56($fp)
	lw	$t0,-8($fp)
	lw	$t1,-56($fp)
	add	$t0,$t0,$t1
	sw	$t0,-60($fp)
	lw	$t0,-60($fp)
	sw	$t0,-8($fp)
	j	L44
L45:
	lw	$t0,12($fp)
	sw	$t0,-16($fp)
	lw	$t0,0($fp)
	sw	$t0,-24($fp)
	lw	$t0,-4($fp)
	sw	$t0,-28($fp)
	lw	$t0,-20($fp)
	sw	$t0,-32($fp)
	lw	$t0,-16($fp)
	sw	$t0,-16($sp)
	lw	$t0,-24($fp)
	sw	$t0,-12($sp)
	lw	$t0,-28($fp)
	sw	$t0,-8($sp)
	lw	$t0,-32($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-12($fp)
	li	$t0,1
	sw	$t0,-12($fp)
	lw	$t0,-4($fp)
	lw	$t1,-12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-16($fp)
	lw	$t0,-16($fp)
	sw	$t0,-4($fp)
	j	L46
L47:
	li	$t0,1
	sw	$t0,-12($fp)
	lw	$t0,0($fp)
	lw	$t1,-12($fp)
	add	$t0,$t0,$t1
	sw	$t0,-16($fp)
	lw	$t0,-16($fp)
	sw	$t0,0($fp)
	j	L48
L49:
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,84
	jr	$ra
main:
	subu	$sp,$sp,1132
	sw	$ra,4($sp)
	sw	$fp,0($sp)
	addiu	$fp,$sp,1128
	li	$t0,3
	sw	$t0,n
	li	$t0,0
	sw	$t0,-116($fp)
	li	$t0,0
	sw	$t0,-124($fp)
	li	$t0,2
	sw	$t0,-132($fp)
	la	$t0,-32($fp)
	sw	$t0,-112($fp)
	lw	$t0,-116($fp)
	sw	$t0,-120($fp)
	lw	$t0,-124($fp)
	sw	$t0,-128($fp)
	lw	$t0,-132($fp)
	sw	$t0,-136($fp)
	lw	$t0,-112($fp)
	sw	$t0,-16($sp)
	lw	$t0,-120($fp)
	sw	$t0,-12($sp)
	lw	$t0,-128($fp)
	sw	$t0,-8($sp)
	lw	$t0,-136($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-108($fp)
	li	$t0,0
	sw	$t0,-148($fp)
	li	$t0,1
	sw	$t0,-156($fp)
	li	$t0,3
	sw	$t0,-164($fp)
	la	$t0,-32($fp)
	sw	$t0,-144($fp)
	lw	$t0,-148($fp)
	sw	$t0,-152($fp)
	lw	$t0,-156($fp)
	sw	$t0,-160($fp)
	lw	$t0,-164($fp)
	sw	$t0,-168($fp)
	lw	$t0,-144($fp)
	sw	$t0,-16($sp)
	lw	$t0,-152($fp)
	sw	$t0,-12($sp)
	lw	$t0,-160($fp)
	sw	$t0,-8($sp)
	lw	$t0,-168($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-140($fp)
	li	$t0,0
	sw	$t0,-180($fp)
	li	$t0,2
	sw	$t0,-188($fp)
	li	$t0,2
	sw	$t0,-196($fp)
	la	$t0,-32($fp)
	sw	$t0,-176($fp)
	lw	$t0,-180($fp)
	sw	$t0,-184($fp)
	lw	$t0,-188($fp)
	sw	$t0,-192($fp)
	lw	$t0,-196($fp)
	sw	$t0,-200($fp)
	lw	$t0,-176($fp)
	sw	$t0,-16($sp)
	lw	$t0,-184($fp)
	sw	$t0,-12($sp)
	lw	$t0,-192($fp)
	sw	$t0,-8($sp)
	lw	$t0,-200($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-172($fp)
	li	$t0,1
	sw	$t0,-212($fp)
	li	$t0,0
	sw	$t0,-220($fp)
	li	$t0,1
	sw	$t0,-228($fp)
	la	$t0,-32($fp)
	sw	$t0,-208($fp)
	lw	$t0,-212($fp)
	sw	$t0,-216($fp)
	lw	$t0,-220($fp)
	sw	$t0,-224($fp)
	lw	$t0,-228($fp)
	sw	$t0,-232($fp)
	lw	$t0,-208($fp)
	sw	$t0,-16($sp)
	lw	$t0,-216($fp)
	sw	$t0,-12($sp)
	lw	$t0,-224($fp)
	sw	$t0,-8($sp)
	lw	$t0,-232($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-204($fp)
	li	$t0,1
	sw	$t0,-244($fp)
	li	$t0,1
	sw	$t0,-252($fp)
	li	$t0,4
	sw	$t0,-260($fp)
	la	$t0,-32($fp)
	sw	$t0,-240($fp)
	lw	$t0,-244($fp)
	sw	$t0,-248($fp)
	lw	$t0,-252($fp)
	sw	$t0,-256($fp)
	lw	$t0,-260($fp)
	sw	$t0,-264($fp)
	lw	$t0,-240($fp)
	sw	$t0,-16($sp)
	lw	$t0,-248($fp)
	sw	$t0,-12($sp)
	lw	$t0,-256($fp)
	sw	$t0,-8($sp)
	lw	$t0,-264($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-236($fp)
	li	$t0,1
	sw	$t0,-276($fp)
	li	$t0,2
	sw	$t0,-284($fp)
	li	$t0,0
	sw	$t0,-292($fp)
	li	$t0,1
	sw	$t0,-296($fp)
	lw	$t0,-292($fp)
	lw	$t1,-296($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-300($fp)
	la	$t0,-32($fp)
	sw	$t0,-272($fp)
	lw	$t0,-276($fp)
	sw	$t0,-280($fp)
	lw	$t0,-284($fp)
	sw	$t0,-288($fp)
	lw	$t0,-300($fp)
	sw	$t0,-304($fp)
	lw	$t0,-272($fp)
	sw	$t0,-16($sp)
	lw	$t0,-280($fp)
	sw	$t0,-12($sp)
	lw	$t0,-288($fp)
	sw	$t0,-8($sp)
	lw	$t0,-304($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-268($fp)
	li	$t0,2
	sw	$t0,-316($fp)
	li	$t0,0
	sw	$t0,-324($fp)
	li	$t0,0
	sw	$t0,-332($fp)
	li	$t0,2
	sw	$t0,-336($fp)
	lw	$t0,-332($fp)
	lw	$t1,-336($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-340($fp)
	la	$t0,-32($fp)
	sw	$t0,-312($fp)
	lw	$t0,-316($fp)
	sw	$t0,-320($fp)
	lw	$t0,-324($fp)
	sw	$t0,-328($fp)
	lw	$t0,-340($fp)
	sw	$t0,-344($fp)
	lw	$t0,-312($fp)
	sw	$t0,-16($sp)
	lw	$t0,-320($fp)
	sw	$t0,-12($sp)
	lw	$t0,-328($fp)
	sw	$t0,-8($sp)
	lw	$t0,-344($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-308($fp)
	li	$t0,2
	sw	$t0,-356($fp)
	li	$t0,1
	sw	$t0,-364($fp)
	li	$t0,1
	sw	$t0,-372($fp)
	la	$t0,-32($fp)
	sw	$t0,-352($fp)
	lw	$t0,-356($fp)
	sw	$t0,-360($fp)
	lw	$t0,-364($fp)
	sw	$t0,-368($fp)
	lw	$t0,-372($fp)
	sw	$t0,-376($fp)
	lw	$t0,-352($fp)
	sw	$t0,-16($sp)
	lw	$t0,-360($fp)
	sw	$t0,-12($sp)
	lw	$t0,-368($fp)
	sw	$t0,-8($sp)
	lw	$t0,-376($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-348($fp)
	li	$t0,2
	sw	$t0,-388($fp)
	li	$t0,2
	sw	$t0,-396($fp)
	li	$t0,0
	sw	$t0,-404($fp)
	li	$t0,3
	sw	$t0,-408($fp)
	lw	$t0,-404($fp)
	lw	$t1,-408($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-412($fp)
	la	$t0,-32($fp)
	sw	$t0,-384($fp)
	lw	$t0,-388($fp)
	sw	$t0,-392($fp)
	lw	$t0,-396($fp)
	sw	$t0,-400($fp)
	lw	$t0,-412($fp)
	sw	$t0,-416($fp)
	lw	$t0,-384($fp)
	sw	$t0,-16($sp)
	lw	$t0,-392($fp)
	sw	$t0,-12($sp)
	lw	$t0,-400($fp)
	sw	$t0,-8($sp)
	lw	$t0,-416($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-380($fp)
	li	$t0,0
	sw	$t0,-428($fp)
	li	$t0,0
	sw	$t0,-436($fp)
	li	$t0,0
	sw	$t0,-444($fp)
	li	$t0,3
	sw	$t0,-448($fp)
	lw	$t0,-444($fp)
	lw	$t1,-448($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-452($fp)
	la	$t0,-68($fp)
	sw	$t0,-424($fp)
	lw	$t0,-428($fp)
	sw	$t0,-432($fp)
	lw	$t0,-436($fp)
	sw	$t0,-440($fp)
	lw	$t0,-452($fp)
	sw	$t0,-456($fp)
	lw	$t0,-424($fp)
	sw	$t0,-16($sp)
	lw	$t0,-432($fp)
	sw	$t0,-12($sp)
	lw	$t0,-440($fp)
	sw	$t0,-8($sp)
	lw	$t0,-456($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-420($fp)
	li	$t0,0
	sw	$t0,-468($fp)
	li	$t0,1
	sw	$t0,-476($fp)
	li	$t0,1
	sw	$t0,-484($fp)
	la	$t0,-68($fp)
	sw	$t0,-464($fp)
	lw	$t0,-468($fp)
	sw	$t0,-472($fp)
	lw	$t0,-476($fp)
	sw	$t0,-480($fp)
	lw	$t0,-484($fp)
	sw	$t0,-488($fp)
	lw	$t0,-464($fp)
	sw	$t0,-16($sp)
	lw	$t0,-472($fp)
	sw	$t0,-12($sp)
	lw	$t0,-480($fp)
	sw	$t0,-8($sp)
	lw	$t0,-488($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-460($fp)
	li	$t0,0
	sw	$t0,-500($fp)
	li	$t0,2
	sw	$t0,-508($fp)
	li	$t0,2
	sw	$t0,-516($fp)
	la	$t0,-68($fp)
	sw	$t0,-496($fp)
	lw	$t0,-500($fp)
	sw	$t0,-504($fp)
	lw	$t0,-508($fp)
	sw	$t0,-512($fp)
	lw	$t0,-516($fp)
	sw	$t0,-520($fp)
	lw	$t0,-496($fp)
	sw	$t0,-16($sp)
	lw	$t0,-504($fp)
	sw	$t0,-12($sp)
	lw	$t0,-512($fp)
	sw	$t0,-8($sp)
	lw	$t0,-520($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-492($fp)
	li	$t0,1
	sw	$t0,-532($fp)
	li	$t0,0
	sw	$t0,-540($fp)
	li	$t0,0
	sw	$t0,-548($fp)
	li	$t0,2
	sw	$t0,-552($fp)
	lw	$t0,-548($fp)
	lw	$t1,-552($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-556($fp)
	la	$t0,-68($fp)
	sw	$t0,-528($fp)
	lw	$t0,-532($fp)
	sw	$t0,-536($fp)
	lw	$t0,-540($fp)
	sw	$t0,-544($fp)
	lw	$t0,-556($fp)
	sw	$t0,-560($fp)
	lw	$t0,-528($fp)
	sw	$t0,-16($sp)
	lw	$t0,-536($fp)
	sw	$t0,-12($sp)
	lw	$t0,-544($fp)
	sw	$t0,-8($sp)
	lw	$t0,-560($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-524($fp)
	li	$t0,1
	sw	$t0,-572($fp)
	li	$t0,1
	sw	$t0,-580($fp)
	li	$t0,0
	sw	$t0,-588($fp)
	li	$t0,4
	sw	$t0,-592($fp)
	lw	$t0,-588($fp)
	lw	$t1,-592($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-596($fp)
	la	$t0,-68($fp)
	sw	$t0,-568($fp)
	lw	$t0,-572($fp)
	sw	$t0,-576($fp)
	lw	$t0,-580($fp)
	sw	$t0,-584($fp)
	lw	$t0,-596($fp)
	sw	$t0,-600($fp)
	lw	$t0,-568($fp)
	sw	$t0,-16($sp)
	lw	$t0,-576($fp)
	sw	$t0,-12($sp)
	lw	$t0,-584($fp)
	sw	$t0,-8($sp)
	lw	$t0,-600($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-564($fp)
	li	$t0,1
	sw	$t0,-612($fp)
	li	$t0,2
	sw	$t0,-620($fp)
	li	$t0,2
	sw	$t0,-628($fp)
	la	$t0,-68($fp)
	sw	$t0,-608($fp)
	lw	$t0,-612($fp)
	sw	$t0,-616($fp)
	lw	$t0,-620($fp)
	sw	$t0,-624($fp)
	lw	$t0,-628($fp)
	sw	$t0,-632($fp)
	lw	$t0,-608($fp)
	sw	$t0,-16($sp)
	lw	$t0,-616($fp)
	sw	$t0,-12($sp)
	lw	$t0,-624($fp)
	sw	$t0,-8($sp)
	lw	$t0,-632($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-604($fp)
	li	$t0,2
	sw	$t0,-644($fp)
	li	$t0,0
	sw	$t0,-652($fp)
	li	$t0,4
	sw	$t0,-660($fp)
	la	$t0,-68($fp)
	sw	$t0,-640($fp)
	lw	$t0,-644($fp)
	sw	$t0,-648($fp)
	lw	$t0,-652($fp)
	sw	$t0,-656($fp)
	lw	$t0,-660($fp)
	sw	$t0,-664($fp)
	lw	$t0,-640($fp)
	sw	$t0,-16($sp)
	lw	$t0,-648($fp)
	sw	$t0,-12($sp)
	lw	$t0,-656($fp)
	sw	$t0,-8($sp)
	lw	$t0,-664($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-636($fp)
	li	$t0,2
	sw	$t0,-676($fp)
	li	$t0,1
	sw	$t0,-684($fp)
	li	$t0,3
	sw	$t0,-692($fp)
	la	$t0,-68($fp)
	sw	$t0,-672($fp)
	lw	$t0,-676($fp)
	sw	$t0,-680($fp)
	lw	$t0,-684($fp)
	sw	$t0,-688($fp)
	lw	$t0,-692($fp)
	sw	$t0,-696($fp)
	lw	$t0,-672($fp)
	sw	$t0,-16($sp)
	lw	$t0,-680($fp)
	sw	$t0,-12($sp)
	lw	$t0,-688($fp)
	sw	$t0,-8($sp)
	lw	$t0,-696($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-668($fp)
	li	$t0,2
	sw	$t0,-708($fp)
	li	$t0,2
	sw	$t0,-716($fp)
	li	$t0,1
	sw	$t0,-724($fp)
	la	$t0,-68($fp)
	sw	$t0,-704($fp)
	lw	$t0,-708($fp)
	sw	$t0,-712($fp)
	lw	$t0,-716($fp)
	sw	$t0,-720($fp)
	lw	$t0,-724($fp)
	sw	$t0,-728($fp)
	lw	$t0,-704($fp)
	sw	$t0,-16($sp)
	lw	$t0,-712($fp)
	sw	$t0,-12($sp)
	lw	$t0,-720($fp)
	sw	$t0,-8($sp)
	lw	$t0,-728($fp)
	sw	$t0,-4($sp)
	jal	set
	sw	$v0,-700($fp)
	la	$t0,-32($fp)
	sw	$t0,-736($fp)
	la	$t0,-68($fp)
	sw	$t0,-740($fp)
	la	$t0,-104($fp)
	sw	$t0,-744($fp)
	lw	$t0,-736($fp)
	sw	$t0,-12($sp)
	lw	$t0,-740($fp)
	sw	$t0,-8($sp)
	lw	$t0,-744($fp)
	sw	$t0,-4($sp)
	jal	matmul
	sw	$v0,-732($fp)
	li	$t0,0
	sw	$t0,-756($fp)
	li	$t0,0
	sw	$t0,-764($fp)
	la	$t0,-104($fp)
	sw	$t0,-752($fp)
	lw	$t0,-756($fp)
	sw	$t0,-760($fp)
	lw	$t0,-764($fp)
	sw	$t0,-768($fp)
	lw	$t0,-752($fp)
	sw	$t0,-12($sp)
	lw	$t0,-760($fp)
	sw	$t0,-8($sp)
	lw	$t0,-768($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-748($fp)
	li	$t0,0
	sw	$t0,-772($fp)
	li	$t0,4
	sw	$t0,-776($fp)
	lw	$t0,-772($fp)
	lw	$t1,-776($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-780($fp)
	lw	$t0,-748($fp)
	lw	$t1,-780($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-784($fp)
	li	$t0,0
	sw	$t0,-796($fp)
	li	$t0,1
	sw	$t0,-804($fp)
	la	$t0,-104($fp)
	sw	$t0,-792($fp)
	lw	$t0,-796($fp)
	sw	$t0,-800($fp)
	lw	$t0,-804($fp)
	sw	$t0,-808($fp)
	lw	$t0,-792($fp)
	sw	$t0,-12($sp)
	lw	$t0,-800($fp)
	sw	$t0,-8($sp)
	lw	$t0,-808($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-788($fp)
	li	$t0,0
	sw	$t0,-812($fp)
	li	$t0,4
	sw	$t0,-816($fp)
	lw	$t0,-812($fp)
	lw	$t1,-816($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-820($fp)
	lw	$t0,-788($fp)
	lw	$t1,-820($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-824($fp)
	lw	$t0,-784($fp)
	beqz	$t0,L52
	lw	$t0,-824($fp)
	beqz	$t0,L50
	li	$t0,1
	sw	$t0,-828($fp)
	j	L51
L50:
	li	$t0,0
	sw	$t0,-828($fp)
L51:
	j	L53
L52:
	li	$t0,0
	sw	$t0,-828($fp)
L53:
	li	$t0,0
	sw	$t0,-840($fp)
	li	$t0,2
	sw	$t0,-848($fp)
	la	$t0,-104($fp)
	sw	$t0,-836($fp)
	lw	$t0,-840($fp)
	sw	$t0,-844($fp)
	lw	$t0,-848($fp)
	sw	$t0,-852($fp)
	lw	$t0,-836($fp)
	sw	$t0,-12($sp)
	lw	$t0,-844($fp)
	sw	$t0,-8($sp)
	lw	$t0,-852($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-832($fp)
	li	$t0,12
	sw	$t0,-856($fp)
	lw	$t0,-832($fp)
	lw	$t1,-856($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-860($fp)
	lw	$t0,-828($fp)
	beqz	$t0,L56
	lw	$t0,-860($fp)
	beqz	$t0,L54
	li	$t0,1
	sw	$t0,-864($fp)
	j	L55
L54:
	li	$t0,0
	sw	$t0,-864($fp)
L55:
	j	L57
L56:
	li	$t0,0
	sw	$t0,-864($fp)
L57:
	li	$t0,1
	sw	$t0,-876($fp)
	li	$t0,0
	sw	$t0,-884($fp)
	la	$t0,-104($fp)
	sw	$t0,-872($fp)
	lw	$t0,-876($fp)
	sw	$t0,-880($fp)
	lw	$t0,-884($fp)
	sw	$t0,-888($fp)
	lw	$t0,-872($fp)
	sw	$t0,-12($sp)
	lw	$t0,-880($fp)
	sw	$t0,-8($sp)
	lw	$t0,-888($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-868($fp)
	li	$t0,0
	sw	$t0,-892($fp)
	li	$t0,15
	sw	$t0,-896($fp)
	lw	$t0,-892($fp)
	lw	$t1,-896($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-900($fp)
	lw	$t0,-868($fp)
	lw	$t1,-900($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-904($fp)
	lw	$t0,-864($fp)
	beqz	$t0,L60
	lw	$t0,-904($fp)
	beqz	$t0,L58
	li	$t0,1
	sw	$t0,-908($fp)
	j	L59
L58:
	li	$t0,0
	sw	$t0,-908($fp)
L59:
	j	L61
L60:
	li	$t0,0
	sw	$t0,-908($fp)
L61:
	li	$t0,1
	sw	$t0,-920($fp)
	li	$t0,1
	sw	$t0,-928($fp)
	la	$t0,-104($fp)
	sw	$t0,-916($fp)
	lw	$t0,-920($fp)
	sw	$t0,-924($fp)
	lw	$t0,-928($fp)
	sw	$t0,-932($fp)
	lw	$t0,-916($fp)
	sw	$t0,-12($sp)
	lw	$t0,-924($fp)
	sw	$t0,-8($sp)
	lw	$t0,-932($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-912($fp)
	li	$t0,0
	sw	$t0,-936($fp)
	li	$t0,18
	sw	$t0,-940($fp)
	lw	$t0,-936($fp)
	lw	$t1,-940($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-944($fp)
	lw	$t0,-912($fp)
	lw	$t1,-944($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-948($fp)
	lw	$t0,-908($fp)
	beqz	$t0,L64
	lw	$t0,-948($fp)
	beqz	$t0,L62
	li	$t0,1
	sw	$t0,-952($fp)
	j	L63
L62:
	li	$t0,0
	sw	$t0,-952($fp)
L63:
	j	L65
L64:
	li	$t0,0
	sw	$t0,-952($fp)
L65:
	li	$t0,1
	sw	$t0,-964($fp)
	li	$t0,2
	sw	$t0,-972($fp)
	la	$t0,-104($fp)
	sw	$t0,-960($fp)
	lw	$t0,-964($fp)
	sw	$t0,-968($fp)
	lw	$t0,-972($fp)
	sw	$t0,-976($fp)
	lw	$t0,-960($fp)
	sw	$t0,-12($sp)
	lw	$t0,-968($fp)
	sw	$t0,-8($sp)
	lw	$t0,-976($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-956($fp)
	li	$t0,9
	sw	$t0,-980($fp)
	lw	$t0,-956($fp)
	lw	$t1,-980($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-984($fp)
	lw	$t0,-952($fp)
	beqz	$t0,L68
	lw	$t0,-984($fp)
	beqz	$t0,L66
	li	$t0,1
	sw	$t0,-988($fp)
	j	L67
L66:
	li	$t0,0
	sw	$t0,-988($fp)
L67:
	j	L69
L68:
	li	$t0,0
	sw	$t0,-988($fp)
L69:
	li	$t0,2
	sw	$t0,-1000($fp)
	li	$t0,0
	sw	$t0,-1008($fp)
	la	$t0,-104($fp)
	sw	$t0,-996($fp)
	lw	$t0,-1000($fp)
	sw	$t0,-1004($fp)
	lw	$t0,-1008($fp)
	sw	$t0,-1012($fp)
	lw	$t0,-996($fp)
	sw	$t0,-12($sp)
	lw	$t0,-1004($fp)
	sw	$t0,-8($sp)
	lw	$t0,-1012($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-992($fp)
	li	$t0,0
	sw	$t0,-1016($fp)
	li	$t0,8
	sw	$t0,-1020($fp)
	lw	$t0,-1016($fp)
	lw	$t1,-1020($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-1024($fp)
	lw	$t0,-992($fp)
	lw	$t1,-1024($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-1028($fp)
	lw	$t0,-988($fp)
	beqz	$t0,L72
	lw	$t0,-1028($fp)
	beqz	$t0,L70
	li	$t0,1
	sw	$t0,-1032($fp)
	j	L71
L70:
	li	$t0,0
	sw	$t0,-1032($fp)
L71:
	j	L73
L72:
	li	$t0,0
	sw	$t0,-1032($fp)
L73:
	li	$t0,2
	sw	$t0,-1044($fp)
	li	$t0,1
	sw	$t0,-1052($fp)
	la	$t0,-104($fp)
	sw	$t0,-1040($fp)
	lw	$t0,-1044($fp)
	sw	$t0,-1048($fp)
	lw	$t0,-1052($fp)
	sw	$t0,-1056($fp)
	lw	$t0,-1040($fp)
	sw	$t0,-12($sp)
	lw	$t0,-1048($fp)
	sw	$t0,-8($sp)
	lw	$t0,-1056($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-1036($fp)
	li	$t0,0
	sw	$t0,-1060($fp)
	li	$t0,15
	sw	$t0,-1064($fp)
	lw	$t0,-1060($fp)
	lw	$t1,-1064($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-1068($fp)
	lw	$t0,-1036($fp)
	lw	$t1,-1068($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-1072($fp)
	lw	$t0,-1032($fp)
	beqz	$t0,L76
	lw	$t0,-1072($fp)
	beqz	$t0,L74
	li	$t0,1
	sw	$t0,-1076($fp)
	j	L75
L74:
	li	$t0,0
	sw	$t0,-1076($fp)
L75:
	j	L77
L76:
	li	$t0,0
	sw	$t0,-1076($fp)
L77:
	li	$t0,2
	sw	$t0,-1088($fp)
	li	$t0,2
	sw	$t0,-1096($fp)
	la	$t0,-104($fp)
	sw	$t0,-1084($fp)
	lw	$t0,-1088($fp)
	sw	$t0,-1092($fp)
	lw	$t0,-1096($fp)
	sw	$t0,-1100($fp)
	lw	$t0,-1084($fp)
	sw	$t0,-12($sp)
	lw	$t0,-1092($fp)
	sw	$t0,-8($sp)
	lw	$t0,-1100($fp)
	sw	$t0,-4($sp)
	jal	get
	sw	$v0,-1080($fp)
	li	$t0,0
	sw	$t0,-1104($fp)
	li	$t0,5
	sw	$t0,-1108($fp)
	lw	$t0,-1104($fp)
	lw	$t1,-1108($fp)
	sub	$t0,$t0,$t1
	sw	$t0,-1112($fp)
	lw	$t0,-1080($fp)
	lw	$t1,-1112($fp)
	seq	$t0,$t0,$t1
	sw	$t0,-1116($fp)
	lw	$t0,-1076($fp)
	beqz	$t0,L80
	lw	$t0,-1116($fp)
	beqz	$t0,L78
	li	$t0,1
	sw	$t0,-1120($fp)
	j	L79
L78:
	li	$t0,0
	sw	$t0,-1120($fp)
L79:
	j	L81
L80:
	li	$t0,0
	sw	$t0,-1120($fp)
L81:
	li	$v0,1
	lw	$t0,-1120($fp)
	move	$a0,$t0
	syscall	
	li	$v0,11
	li	$a0,10
	syscall	
	lw	$fp,0($sp)
	lw	$ra,4($sp)
	addiu	$sp,$sp,1132
	jr	$ra