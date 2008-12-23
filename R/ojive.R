`ojive` <-
function(h, w, hmin, hmax, wmin, wmax, a, r1, b, r2, rhwsumhh, rhwsumhi, lhwsumhh, lhwsumhi, rwhsumww, rwhsumwi, lwhsumww, lwhsumwi, ihwr1, ihwr2, ihwl1, ihwl2, iwhr1, iwhr2, iwhl1, iwhl2) {
#OJIVFIT
#     Fit an o-jive shape to the influence functions for H and W.
#     By extension, we will get an o-jive shaped nullcline for each.
#-Start:
# Husb positive affect range
  if (length(h[h>0])) { 
     # POSHALF
      hpos<- poshalf(hmax, length(h[h>0]), ihwr1, ihwr2)
  }

# Husb negative affect range
   if (length(h[h<0])) {
       hneg <- neghalf(hmin, length(h[h<0]), ihwl1, ihwl2)
   }

# Wife positive affect range
   if (length(w[w>0])) {
	wpos <- poshalf(wmax, length(w[w>0]), iwhr1, iwhr2)
   }
# Wife negative affect range
   if (length(w[w<0])) {
        wneg <- neghalf(wmin, length(w[w<0]), iwhl1, iwhl2)
   }
# OJSETPT
#     Find influenced setpoints from the nullcline plots (if any)
#     for the o-jive shaped influence function#
# The following reference the horizontal and vertical segments of the
# nullcline plots:
#     hvx =       x-coord of Husband's nullcline verticals
#     hvy =       y-coord of endpts of Husband's nullcline verticals
#     whx =       x-coord of endpts of Wife's nullcline horizontals
#     why =       y-coord of Wife's nullcline horizontals

#     wvx =       x-coord of Wife's nullcline verticals
#     wvy =       y-coord of endpts of Wife's nullcline verticals
#     hhx =       x-coord of endpts of Husband's nullcline horizontals
#     hhy =       y-coord of Husband's nullcline horizontals
   hsetpoints <- rep(NA, 4);
   wsetpoints <- rep(NA, 4);
   
#  Wife's nullcline, W = (Ihw(H) + a)/(1-r1)
   aw <- (hpos$ax + a)/(1-r1)
   bw <- (hpos$bx + a)/(1-r1)
   cw <- hpos$cx
   dw <- (hneg$dx + a)/(1-r1)  
   ew <- (hneg$ex + a)/(1-r1)  
   fw <- hneg$fx

#  Husband's nullcline, H <- (Iwh(W) + b)/(1-r2)
      ah <- (wpos$ax + b)/(1-r2)  
      bh <- (wpos$bx + b)/(1-r2)
      ch <- wpos$cx
      dh <- (wneg$dx + b)/(1-r2)  
      eh <- (wneg$ex + b)/(1-r2)  
      fh <- wneg$fx

     hvx <- rep(0,4)
     why <- rep(0,4)

     wvx <- rep(0,2)
     hhy <- rep(0,2)
     wvy1 <- rep(0,2)
     wvy2 <- rep(0,2)

     hvy1 <- rep(0,4)
     hvy2 <- rep(0,4)
     whx1 <- rep(0,4)
     whx2 <- rep(0,4)
     hhx1 <-  rep(0,2)
     hhx2 <-  rep(0,2)
     wife <-NA
     husb <-NA

# Husband Verticals:     

      hvx[1] <-    eh
      hvy1[1] <-  wmin
      hvy2[1] <-  fh

      hvx[2] <-    dh
      hvy1[2] <-  fh
      hvy2[2] <-  0

      hvx[3] <-    ah
      hvy1[3] <-  0
      hvy2[3] <-  ch

      hvx[4] <-    bh
      hvy1[4] <-  ch
      hvy2[4] <-  wmax

# Wife Horizontals:
      why[1] <-    ew
      whx1[1] <-  hmin
      whx2[1] <-  fw

      why[2] <-    dw
      whx1[2] <-  fw
      whx2[2] <-  0

      why[3] <-    aw
      whx1[3] <-  0
      whx2[3] <-  cw

      why[4] <-    bw
      whx1[4] <-  cw
      whx2[4] <-  hmax

# Wife Verticals:
      wvx[1] <-    fw
      if(dw < ew) {
          wvy1[1] <-  dw
          wvy1[2] <-  ew
        } else {
          wvy1[1] <-  ew
          wvy1[2] <-  dw
       }

      wvx[2] <-    cw
      if(aw < bw)  {
          wvy2[1] <-  aw
          wvy2[2] <-  bw
        } else {
          wvy2[1] <-  bw
          wvy2[2] <-  aw
       }

# Husband Horizontals:
      hhy[1] <-    fh
      if(dh < eh){
          hhx1[1] <-  dh
          hhx2[1] <-  eh
        } else {
          hhx1[1] <-  eh
          hhx2[1] <-  dh
       }

      hhy[2] <-    ch
      if(ah < bh) {
          hhx1[2] <-  ah
          hhx2[2] <-  bh
      } else {
          hhx1[2] <-  bh
          hhx2[2] <-  ah
       }
#-Start:

# Compare Husband verticals to Wife horizontals to see if they cross
      for (v in 1:4) {
        for(h in 1:4) {
        if( whx1[h] <= hvx[v] &  whx2[h] >= hvx[v] & 
             (hvy1[v] <= why[h] & hvy2[v] >= why[h])) {
          wife = why[h]
          husb = hvx[v]
#         Find the quadrant and store the setpoint
          if(husb < 0 & wife > 0  & is.na(wsetpoints[1])) {
                wsetpoints[1] <- wife;
                hsetpoints[1] <- husb;
          }
          if(husb < 0 & wife < 0  & is.na(wsetpoints[2])) {
                wsetpoints[2] <- wife;
                hsetpoints[2] <- husb;
	  }
          if (husb > 0 &  wife > 0  & is.na(wsetpoints[3])) {
                wsetpoints[3] <- wife;
                hsetpoints[3] <- husb;
          } 
          if(husb > 0 & wife < 0 & is.na(wsetpoints[4])) {
                wsetpoints[4] <- wife;
                hsetpoints[4] <- husb;
          }
        }
      }
     }


# Compare Wife verticals to Husband horizontals to see if they cross
      for (v in 1:2) {
      for( h in 1:2) {
        if( (hhx1[h] < wvx[v] &  hhx2[h] >= wvx[v] &
             wvy1[v] <= hhy[h] &  wvy2[v] >= hhy[h])) {
          wife <- hhy[h]
          husb <- wvx[v]
#         Find the quadrant and store the setpoint
          if(husb < 0 & wife > 0 & is.na(wsetpoints[1])) {
              wsetpoints[1] <- wife;
              hsetpoints[1] <- husb;
          }
          if(husb < 0 & wife < 0 & is.na(wsetpoints[2])) {
              wsetpoints[2] <- wife;
              hsetpoints[2] <- husb;
          }
          if(husb > 0 & wife > 0 & is.na(wsetpoints[3])) {
              wsetpoints[3] <- wife;
              hsetpoints[3] <- husb;
          }
          if(husb > 0 & wife < 0 & is.na(wsetpoints[4])) {
              wsetpoints[4] <- wife;
              hsetpoints[4] <- husb;
          }
     }
   }
  }



return(list(ah=ah, bh=bh, ch=ch, dh=dh, eh=eh, fh=fh, aw=aw, bw=bw, cw=cw, dw=dw, ew=ew, fw=fw, hsetpoints=hsetpoints, wsetpoints=wsetpoints))

}

