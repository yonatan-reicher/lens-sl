    mov	r2, r0
	mov	r3, r1
	and	r3, r2, r3
	mov	r4, r3
	mov	r2, r0
	mov	r3, r1
	eor	r3, r2, r3
	mov	r2, r3
	mov	r3, r4
	cmp	r2, r3
	movhi	r3, #0
	movls	r3, #1
	mov	r0, r3
