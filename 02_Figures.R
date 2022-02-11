library(tidyverse)
library(data.table)
library(tidytable)
library(ggplot2)
library(ggbeeswarm)
library(ggforce)
library(RColorBrewer)
library(miceadds)
library(pscl)

#### Facet Zoom2 ####
facet_zoom2 <- function(x, y, xy, zoom.data, xlim = NULL, ylim = NULL, 
                        split = FALSE, horizontal = TRUE, zoom.size = 2, 
                        show.area = TRUE, shrink = TRUE) {
  x <- if (missing(x)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(x)
  y <- if (missing(y)) if (missing(xy)) NULL else lazyeval::lazy(xy) else lazyeval::lazy(y)
  zoom.data <- if (missing(zoom.data)) NULL else lazyeval::lazy(zoom.data)
  if (is.null(x) && is.null(y) && is.null(xlim) && is.null(ylim)) {
    stop("Either x- or y-zoom must be given", call. = FALSE)
  }
  if (!is.null(xlim)) x <- NULL
  if (!is.null(ylim)) y <- NULL
  ggproto(NULL, FacetZoom2,
          shrink = shrink,
          params = list(
            x = x, y = y, xlim = xlim, ylim = ylim, split = split, zoom.data = zoom.data,
            zoom.size = zoom.size, show.area = show.area,
            horizontal = horizontal
          )
  )
}

FacetZoom2 <- ggproto(
  "FacetZoom2",
  ggforce::FacetZoom,
  
  compute_layout = function(data, params) {
    layout <- rbind( # has both x & y dimension
      data.frame(name = 'orig', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'x', SCALE_X = 2L, SCALE_Y = 1L),
      data.frame(name = 'y', SCALE_X = 1L, SCALE_Y = 2L),
      data.frame(name = 'full', SCALE_X = 2L, SCALE_Y = 2L),
      data.frame(name = 'orig_true', SCALE_X = 1L, SCALE_Y = 1L),
      data.frame(name = 'zoom_true', SCALE_X = 1L, SCALE_Y = 1L)
    )
    if (is.null(params$y) && is.null(params$ylim)) { # no y dimension
      layout <- layout[c(1,2, 5:6),]
    } else if (is.null(params$x) && is.null(params$xlim)) { # no x dimension
      layout <- layout[c(1,3, 5:6),]
    }
    layout$PANEL <- seq_len(nrow(layout))
    layout
  },
  
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord,
                         data, theme, params) {
    
    if (is.null(params$x) && is.null(params$xlim)) {
      params$horizontal <- TRUE
    } else if (is.null(params$y) && is.null(params$ylim)) {
      params$horizontal <- FALSE
    }
    if (is.null(theme[['zoom']])) {
      theme$zoom <- theme$strip.background
    }
    if (is.null(theme$zoom.x)) {
      theme$zoom.x <- theme$zoom
    }
    if (is.null(theme$zoom.y)) {
      theme$zoom.y <- theme$zoom
    }
    axes <- render_axes(ranges, ranges, coord, theme, FALSE)
    panelGrobs <- ggforce:::create_panels(panels, axes$x, axes$y)
    panelGrobs <- panelGrobs[seq_len(length(panelGrobs) - 2)]
    if ('full' %in% layout$name && !params$split) {
      panelGrobs <- panelGrobs[c(1, 4)]
    }
    
    # changed coordinates in indicator / lines to zoom from 
    # the opposite horizontal direction
    if ('y' %in% layout$name) {
      if (!inherits(theme$zoom.y, 'element_blank')) {
        zoom_prop <- scales::rescale(
          y_scales[[2]]$dimension(ggforce:::expansion(y_scales[[2]])),
          from = y_scales[[1]]$dimension(ggforce:::expansion(y_scales[[1]])))
        indicator <- polygonGrob(
          x = c(0, 0, 1, 1), # was x = c(1, 1, 0, 0), 
          y = c(zoom_prop, 1, 0), 
          gp = gpar(col = NA, fill = alpha(theme$zoom.y$fill, 0.5)))
        lines <- segmentsGrob(
          x0 = c(1, 1), x1 = c(0, 0), # was x0 = c(0, 0), x1 = c(1, 1)
          y0 = c(0, 1), y1 = zoom_prop,
          gp = gpar(col = theme$zoom.y$colour,
                    lty = theme$zoom.y$linetype,
                    lwd = theme$zoom.y$size,
                    lineend = 'round'))
        indicator_h <- grobTree(indicator, lines)
      } else {
        indicator_h <- zeroGrob()
      }
    }
    
    if ('x' %in% layout$name) {
      if (!inherits(theme$zoom.x, 'element_blank')) {
        zoom_prop <- scales::rescale(x_scales[[2]]$dimension(ggforce:::expansion(x_scales[[2]])),
                                     from = x_scales[[1]]$dimension(ggforce:::expansion(x_scales[[1]])))
        indicator <- polygonGrob(c(zoom_prop, 1, 0), c(1, 1, 0, 0), 
                                 gp = gpar(col = NA, fill = alpha(theme$zoom.x$fill, 0.5)))
        lines <- segmentsGrob(x0 = c(0, 1), y0 = c(0, 0), x1 = zoom_prop, y1 = c(1, 1), 
                              gp = gpar(col = theme$zoom.x$colour,
                                        lty = theme$zoom.x$linetype,
                                        lwd = theme$zoom.x$size,
                                        lineend = 'round'))
        indicator_v <- grobTree(indicator, lines)
      } else {
        indicator_v <- zeroGrob()
      }
    }
    
    if ('full' %in% layout$name && params$split) {
      space.x <- theme$panel.spacing.x
      if (is.null(space.x)) space.x <- theme$panel.spacing
      space.x <- unit(5 * as.numeric(convertUnit(space.x, 'cm')), 'cm')
      space.y <- theme$panel.spacing.y
      if (is.null(space.y)) space.y <- theme$panel.spacing
      space.y <- unit(5 * as.numeric(convertUnit(space.y, 'cm')), 'cm')
      
      # change horizontal order of panels from [zoom, original] to [original, zoom]
      # final <- gtable::gtable_add_cols(panelGrobs[[3]], space.x)
      # final <- cbind(final, panelGrobs[[1]], size = 'first')
      # final_tmp <- gtable::gtable_add_cols(panelGrobs[[4]], space.x)
      # final_tmp <- cbind(final_tmp, panelGrobs[[2]], size = 'first')
      final <- gtable::gtable_add_cols(panelGrobs[[1]], space.x)
      final <- cbind(final, panelGrobs[[3]], size = 'first')
      final_tmp <- gtable::gtable_add_cols(panelGrobs[[2]], space.x)
      final_tmp <- cbind(final_tmp, panelGrobs[[4]], size = 'first')
      
      final <- gtable::gtable_add_rows(final, space.y)
      final <- rbind(final, final_tmp, size = 'first')
      final <- gtable::gtable_add_grob(final, list(indicator_h, indicator_h),
                                       c(2, 6), 3, c(2, 6), 5,
                                       z = -Inf, name = "zoom-indicator")
      final <- gtable::gtable_add_grob(final, list(indicator_v, indicator_v), 
                                       3, c(2, 6), 5, 
                                       z = -Inf, name = "zoom-indicator")
      heights <- unit.c(
        unit(max_height(list(axes$x[[1]]$top, axes$x[[3]]$top)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$x[[1]]$bottom, axes$x[[3]]$bottom)), 'cm'),
        space.y,
        unit(max_height(list(axes$x[[2]]$top, axes$x[[4]]$top)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$x[[2]]$bottom, axes$x[[4]]$bottom)), 'cm')
      )
      
      # swop panel width specifications according to the new horizontal order
      widths <- unit.c(
        # unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        # unit(params$zoom.size, 'null'),
        # unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm'),
        # space.x,
        # unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        # unit(1, 'null'),
        # unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')        
        unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
        unit(1, 'null'),
        unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm'),
        space.x,
        unit(max_width(list(axes$y[[3]]$left, axes$y[[4]]$left)), 'cm'),
        unit(params$zoom.size, 'null'),
        unit(max_height(list(axes$y[[3]]$right, axes$y[[4]]$right)), 'cm')
        
      )
      final$heights <- heights
      final$widths <- widths
    } else {
      if (params$horizontal) {
        space <- theme$panel.spacing.x
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        heights <- unit.c(
          unit(max_height(list(axes$x[[1]]$top, axes$x[[2]]$top)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$x[[1]]$bottom, axes$x[[2]]$bottom)), 'cm')
        )
        
        # change horizontal order of panels from [zoom, original] to [original, zoom]
        # first <- gtable::gtable_add_cols(panelGrobs[[2]], space)
        # first <- cbind(final, panelGrobs[[1]], size = 'first')
        final <- gtable::gtable_add_cols(panelGrobs[[1]], space) 
        final <- cbind(final, panelGrobs[[2]], size = "first") 
        
        final$heights <- heights
        
        # swop panel width specifications according to the new horizontal order
        # unit(c(params$zoom.size, 1), 'null')
        final$widths[panel_cols(final)$l] <- unit(c(1, params$zoom.size), 'null') 
        
        final <- gtable::gtable_add_grob(final, indicator_h, 2, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      } else {
        space <- theme$panel.spacing.y
        if (is.null(space)) space <- theme$panel.spacing
        space <- unit(5 * as.numeric(convertUnit(space, 'cm')), 'cm')
        widths <- unit.c(
          unit(max_width(list(axes$y[[1]]$left, axes$y[[2]]$left)), 'cm'),
          unit(1, 'null'),
          unit(max_height(list(axes$y[[1]]$right, axes$y[[2]]$right)), 'cm')
        )
        final <- gtable::gtable_add_rows(panelGrobs[[1]], space)
        final <- rbind(final, panelGrobs[[2]], size = 'first')
        final$widths <- widths
        final$heights[panel_rows(final)$t] <- unit(c(1, params$zoom.size), 'null')
        final <- gtable::gtable_add_grob(final, indicator_v, 3, 2, 5, 
                                         z = -Inf, name = "zoom-indicator")
      }
    }
    final
  }
)

#### Analysis ####

core <- readRDS(file="data/core-2019.rds")
hosp <- readRDS(file="data/hosp-2019.rds")

library(survey)
# Set up design
cluster <- svydesign(
  id=~key_ed,
  strata=~neds_stratum,
  weights=~discwt,
  nest=TRUE,
  data=core,
  multicore=T)

svymean(~age, cluster,na.rm=T,se=TRUE,multicore=T)

# Give information about hospitals with outcome rate 0%
hospital_data <- core %>%
  group_by(hosp_ed) %>%
  summarise(
    Obs_Rate = sum(ifelse(admit=="Observation Billing",1,0))/n(),
    Admit_Rate = sum(ifelse(admit=="Admit",1,0))/n(),
    Transfer_Rate = sum(ifelse(admit=="Transfer",1,0))/n(),
    Outcome_Rate = Obs_Rate+Admit_Rate+Transfer_Rate,
    Low_Risk_Syncope_Visits = n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  select(-hosp_control,-hosp_urban,-hosp_teach)
hospital_data %>% filter(Outcome_Rate==0) %>% summary()

# Plot admissions across hospitals (figure 2)
core %>%
  group_by(hosp_ed) %>%
  summarise(Admit_Rate = sum(ifelse(admit!="Discharge/Transfer to SNF/Other/Died in ED",1,0))/n()) %>%
  left_join(hosp,by="hosp_ed") %>%
  # Drop if admit rate is 0
  filter(Admit_Rate>0.00000001 & 
           Admit_Rate!=1) %>%
  mutate(Hospital = "") %>%
  ggplot(aes(Hospital,Admit_Rate))+
  geom_boxplot(alpha=0.7,outlier.shape=NA)+
  geom_quasirandom(alpha=0.5)+
  scale_color_brewer(type="seq",palette=1)+
  theme_bw()+ylab("Hospitalization Rate")+xlab("")+
  labs(colour="Yearly Visits Category")+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  facet_zoom2(ylim=c(0,0.07),zoom.size=1)#+
  #ggtitle("Admit Rates for Syncope by Hospital-Based ED (237,295 records)")
ggsave("Figure2.jpeg",width=6,height=5,dpi=600)


# Build logistic regression model and plot ORs
# model_input <- core %>%
#   left_join(hosp,by="hosp_ed") %>%
#   mutate(outcome = as.integer(ifelse(admit=="Admit" | admit=="Transfer" | admit=="Observation Billing",1,0))) %>%
#   group_by(hosp_ed) %>% mutate(hosp_admit_rate = sum(outcome)/n()) %>% ungroup() %>%
#   filter(hosp_admit_rate!=1 & hosp_admit_rate!=0) %>% select(-hosp_admit_rate)
# model <- glm.cluster(data=model_input,outcome~visits_category,cluster="hosp_ed",family=binomial)
# 
# output <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))[1:5,]
# rownames(output) <- NULL
# 
# output[1,4] <- "<20k"
# output[2,4] <- "20-40k"
# output[3,4] <- "40-60k"
# output[4,4] <- "60-80k"
# output[5,4] <- "80k+"
# 
# colnames(output) <- c("OR","CI_low","CI_high","Labels")
# 
# output <- output %>%
#   mutate(OR = as.numeric(OR),
#          CI_low = as.numeric(CI_low),
#          CI_high = as.numeric(CI_high)) %>%
#   mutate(OR = case_when(
#     Labels=="<20k" ~ as.numeric(1),T~OR)) %>%
#   mutate(CI_high = case_when(
#     Labels=="<20k" ~ NaN,T~OR)) %>%
#   mutate(CI_low = case_when(
#     Labels=="<20k" ~ NaN,T~OR)) %>%
#   mutate(Labels = factor(Labels,levels=c(
#     "<20k","20-40k","40-60k","60-80k","80k+")))
# 
# output %>%
#   ggplot(aes(x=OR,y=Labels))+
#   geom_vline(aes(xintercept=1),size=0.25,linetype='dashed')+
#   geom_point(size=4)+
#   geom_errorbarh(aes(xmax=CI_high,xmin=CI_low),size=0.5,height=0.2,color="gray50")+
#   scale_x_continuous(trans='log2',breaks=c(0.125,0.5,1,2,8),labels=c("1/8","1/2","","2","8"))+
#   xlab("Odds Ratios (95% Confidence Interval)")+
#   ylab("")+
#   theme_bw()+
#   coord_cartesian(xlim=c(0.1,10))

# Build logistic regression model and plot ORs
model_input <- core %>%
  mutate(age_category = case_when(
    age<30 ~ "18 to 29",
    age>=30 & age<40 ~ "30 to 39",
    T ~ "40 to 49")) %>%
  left_join(hosp,by="hosp_ed") %>%
  mutate(outcome = as.integer(ifelse(admit=="Admit",1,0))) %>%
  group_by(hosp_ed) %>% mutate(hosp_admit_rate = sum(outcome)/n()) %>% ungroup() %>%
  filter(hosp_admit_rate!=1) %>% select(-hosp_admit_rate)

# Set factor variable levels
model_input <- model_input %>%
  mutate(hosp_control = relevel(hosp_control,ref = "Government, nonfederal (public)"))

model <- glm.cluster(data=model_input,outcome ~ age_category+hosp_control+hosp_urban+hosp_teach+visits_category,cluster="hosp_ed",family="binomial")

library(performance)
r2_nagelkerke(model$glm_res)

Gvif <- function(FitglmC){
  # Terms in the model
  str<-as.character(FitglmC$glm_res$formula)
  str<-gsub("\n","",str,fixed=TRUE)
  str<-gsub(" ","",str,fixed=TRUE)
  Fit.terms<-strsplit(str, "\\+")[[3]]
  Fit.names<-names(FitglmC$glm_res$coefficients)
  x<-cov2cor(vcov(FitglmC))
  # Remove the intercept
  x<-x[-1,-1]
  # Calculations
  R<-det(x)
  for (v in 1:length(Fit.terms)){
    sub<-grepl(Fit.terms[[v]],Fit.names[-1])
    nsub<-!sub
    Df<-sum(sub)
    if (Df>1){
      R11<-det(x[sub,sub])
      R22<-det(x[nsub,nsub])
      vif<-R11*R22/R
    } else {
      R11<-x[sub,sub]
      R22<-det(x[nsub,nsub])
      vif<-R11*R22/R
    }
    gvif<-vif^(1/(2*Df))
    print(sprintf("%30s %2d %8.4f %8.4f 
          %8.3f",Fit.terms[[v]],Df,vif,gvif,gvif^2))
  }
}

output <- as.data.frame(exp(cbind(OR = coef(model), confint(model))))[4:17,]
rownames(output) <- NULL

# Reference Government or private (collapsed category)
output[1,4] <- "Government of private (collapsed)"
output[2,4] <- "Private, not-for-profit (voluntary)"
output[3,4] <- "Private, investor-owned (proprietary)"
output[4,4] <- "Private (collapsed category)"
# Reference Large metro
output[5,4] <- "Small metro"
output[6,4] <- "Micropolitan"
output[7,4] <- "Not metropolitan or micropolitan"
output[8,4] <- "Collapsed categories/other"
# Refeence Metropolitan non-teaching
output[9,4] <- "Metropolitan teaching"
output[10,4] <- "Non-metropolitan hospital"
# Reference <20k
output[11,4] <- "20-40k"
output[12,4] <- "40-60k"
output[13,4] <- "60-80k"
output[14,4] <- "80k+"

colnames(output) <- c("OR","CI_low","CI_high","Labels")

# Add in reference categories
output <- rbind(
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Government, nonfederal (public)"))),
  (output[1:4,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Large metro"))),
  (output[5:8,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="Metropolitan non-teaching"))),
  (output[9:10,]),
  (as.data.frame(cbind(OR=1,CI_low=NA_real_,CI_high=NA_real_,Labels="<20k"))),
  (output[11:14,]))

output <- output %>%
  mutate(OR = as.numeric(OR),
         CI_low = as.numeric(CI_low),
         CI_high = as.numeric(CI_high)) %>%
  mutate(Labels = factor(Labels,levels=c(
    "Government, nonfederal (public)",
    "Government of private (collapsed category)",
    "Private, not-for-profit (voluntary)",
    "Private, investor-owned (proprietary)",
    "Private (collapsed category)",
    "Large metro",
    "Small metro",
    "Micropolitan",
    "Not metropolitan or micropolitan",
    "Collapsed categories/other",
    "Metropolitan non-teaching",
    "Metropolitan teaching",
    "Non-metropolitan hospital",
    "<20k",
    "20-40k",
    "40-60k",
    "60-80k",
    "80k+"))) %>%
  mutate(Category= case_when(
    Labels=="Metropolitan non-teaching" |
    Labels== "Metropolitan teaching" |
    Labels== "Non-metropolitan hospital"~ "Teaching Status",
    Labels=="Large metro" |
    Labels=="Small metro" |
    Labels=="Micropolitan" |
    Labels=="Not metropolitan or micropolitan" |
    Labels=="Collapsed categories/other" ~ "Urban-Rural",
    Labels=="Government or private (collapsed category)" |
    Labels=="Government, nonfederal (public)" |
    Labels=="Private, not-for-profit (voluntary)" |
    Labels=="Private, investor-owned (proprietary)" |
    Labels=="Private (collapsed category)" ~ "Ownership\nStatus",
    T ~ "Yearly Visits")) %>%
  mutate(Category=factor(Category,levels=c(
    "Yearly Visits",
    "Teaching Status",
    "Urban-Rural",
    "Ownership\nStatus"
  )))

output %>%
  mutate(Labels=
           case_when(
             Labels=="<20k" ~ "*<20k",
             Labels=="Metropolitan non-teaching" ~ "*Metropolitan non-teaching",
             Labels=="Large metro" ~ "*Large metro",
             Labels=="Government or private (collapsed category)"~"*Government or private (collapsed category)",
             T~as.character(Labels)
           )) %>%
  mutate(Labels = factor(Labels,levels=c(
    "*Government or private (collapsed category)",
    "Government, nonfederal (public)",
    "Private, not-for-profit (voluntary)",
    "Private, investor-owned (proprietary)",
    "Private (collapsed category)",
    "*Large metro",
    "Small metro",
    "Micropolitan",
    "Not metropolitan or micropolitan",
    "Collapsed categories/other",
    "*Metropolitan non-teaching",
    "Metropolitan teaching",
    "Non-metropolitan hospital",
    "*<20k",
    "20-40k",
    "40-60k",
    "60-80k",
    "80k+"))) %>%
  ggplot(aes(x=OR,y=Labels,colour=Category))+
  geom_vline(aes(xintercept=1),size=0.25,linetype='dashed')+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax=CI_high,xmin=CI_low),size=0.5,height=0.2,color="gray50")+
  scale_x_continuous(trans='log2',breaks=c(0.125,0.5,1,2,8),labels=c("1/8","1/2","","2","8"))+
  xlab("Odds Ratios (95% Confidence Interval)")+
  ylab("")+
  theme_bw()+
  coord_cartesian(xlim=c(0.1,10))+
  labs(caption = "* indicates reference category")
ggsave("Figure3.jpeg",width=9,height=5,dpi=600)


