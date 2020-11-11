import gl
gl.resetdefaults()
gl.meshhemisphere(1)

#gl.meshloadbilateral(1)
#gl.hemispheredistance(0.5)
gl.meshload("/Users/chris/aa/_rh.inflated")

gl.overlayload('fs7converted_fs5_sct2in_fwhm10_npc_cft3.1_rh_dpv_npc_fisher_cfdrp_d1_c1.mgz')
gl.overlayminmax(1,2.301,4)
gl.overlaycolor(1,128,255,128,255,0,255)

gl.overlayload('fs7converted_fs5_sct2in_fwhm10_npc_cft3.1_rh_dpv_npc_fisher_cfdrp_d1_c2.mgz')
gl.overlayminmax(2,2.301,4)
gl.overlaycolor(2,255,0,255,128,128,128)

gl.overlayload('fs7converted_fs5_sct2in_fwhm10_npc_cft3.1_rh_dpv_npc_fisher_cfdrp_d1_c3.mgz')
gl.overlayminmax(3,2.301,4)
gl.overlaycolor(3,255,0,0,128,255,128)

gl.overlayload('fs7converted_fs5_sct2in_fwhm10_npc_cft3.1_rh_dpv_npc_fisher_cfdrp_d1_c4.mgz')
gl.overlayminmax(4,2.301,4)
gl.overlaycolor(4,0,0,255,128,255,128)

gl.shadername('mixmatch')
gl.viewsagittal(1)
gl.colorbarvisible(1)
gl.orientcubevisible(0)
gl.overlayadditive(0)