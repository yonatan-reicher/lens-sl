	mov	r3, r0
	mov	r3, r3, asr #31
	mov	r2, r3
	mov	r3, r0
	rsb	r3, r3, #0
	mov	r3, r3, asr #31
	orr	r3, r2, r3
	mov	r0, r3
