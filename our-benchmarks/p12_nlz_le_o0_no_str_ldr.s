	mov	r3, r1
	mvn	r3, r3
	mov	r2, r3
	mov	r3, r0
	and	r3, r2, r3
	mov	r2, r3
	mov	r3, r1
	cmp	r2, r3
	movhi	r3, #0
	movls	r3, #1
	mov	r0, r3
