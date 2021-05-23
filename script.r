source('./r_files/flatten_HTML.r')

############### Library Declarations ###############
libraryRequireInstall("ggplot2");
libraryRequireInstall("plotly")
libraryRequireInstall("viridisLite")
libraryRequireInstall("viridis")


#####################################################
#################### Actual code ####################


#####################################################
########## Incicializacion variables#################

if (!exists("graphic_type_graphic_type"))
{
    graphic_type_graphic_type = "boxplot";
}
if (!exists("settings_Scatter_tipo_Scatter"))
{
    settings_Scatter_tipo_Scatter = "Point";
}
if (!exists("settings_Scatter_metodo"))
{
    settings_Scatter_metodo= "loess";
}
if (!exists("settings_Global_Disposicion"))
{
    settings_Global_Disposicion = "Vertical";
}
if (!exists("settings_Scatter_shape"))
{
    settings_Scatter_shape = "TRUE";
}
if (!exists("settings_Box_Violin_trim"))
{
    settings_Box_Violin_trim = "TRUE";
}
if (!exists("settings_Global_colour")) 
{
    settings_Global_colour = "dodgerblue";
}
if (!exists("settings_Global_themes")) 
{
    settings_Global_themes = "theme_bw";
}
if (!exists("settings_Global_size"))
{
    settings_Global_size = 1;
}
if (!exists("settings_Global_fill"))
{
    settings_Global_fill = "RdPu";
}
if (!exists("settings_Box_Violin_notch"))
{
    settings_Box_Violin_notch = TRUE;
}
if (!exists("settings_Box_Violin_jitter"))
{
    settings_Box_Violin_jitter = FALSE;
}

stop_work=FALSE



    
    if (!exists("eje_x") || !exists("eje_y") )
    {
            eje_x=0
            eje_y=0
            eje_z=0

            dataset=data.frame(eje_x,eje_y)
            
            p2=ggplot(data = dataset, aes(x = eje_x, y = eje_y))+ scale_x_continuous(breaks=seq(0, 1, 1))+ scale_y_continuous(breaks=seq(0, 1, 1)) +  ggtitle("Graphic not available.
            
                It is necessary 
                to indicate 
                the elements 
                of the axes.")                 
                p2 = ggplotly(p2);
                internalSaveWidget(p2, 'out.html');
                stop_work==TRUE
            
    }else
    {
        if (!exists("eje_z"))
        {
            dataset = data.frame(eje_x, eje_y)
            eje_z3 = ""
            eje_x1 = dataset[,1]
            n1=names(dataset[1])
            eje_y2 = dataset[,2]
            n2=names(dataset[2])
            fact_x=factor(eje_x1)
            eje_x1_f=factor(eje_x1)
            eje_y2_f=factor(eje_y2)
        }else
        {

            dataset = data.frame(eje_x, eje_y,eje_z)
            eje_x1 = dataset[,1]
            n1=names(dataset[1])
            eje_y2 = dataset[,2]
            n2=names(dataset[2])
            eje_z3 = dataset[,3]
            n3=names(dataset[3])
            fact_x=factor(eje_x1)
            eje_x1_f=factor(eje_x1)
            eje_y2_f=factor(eje_y2)
        }


        if (stop_work==FALSE)
        {
                    p=""
                    settings_Global_fill_f="NoPalette"

                    if (settings_Global_fill=="firebrick2" || settings_Global_fill=="Blues" || settings_Global_fill=="lightgreen" || settings_Global_fill=="lightyellow")
                    {
                        settings_Global_fill_f="NoPalette"
                    } else
                    {
                        settings_Global_fill_f="Palette"
                    }


                    if (graphic_type_graphic_type=='boxplot')
                    {
                        if (settings_Box_Violin_jitter=='TRUE')
                        {
                            if (settings_Global_fill_f=='Palette')
                            {
                                val=fact_x
                                p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                geom_boxplot( aes(fill = val),notch = settings_Box_Violin_notch)+
                                xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) +  geom_jitter(size=settings_Global_size,colour =settings_Global_colour) 
                            } else
                            {
                                p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                geom_boxplot(aes(x = factor(eje_x1)),fill = settings_Global_fill,notch = settings_Box_Violin_notch)+
                                +  geom_jitter(size=settings_Global_size,colour =settings_Global_colour) 
                            }
                        
                        } else
                        {
                            if (settings_Global_fill_f=='Palette')
                                {
                                    val=fact_x
                                    p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                    geom_boxplot(aes(fill = val),name=n2,notch = settings_Box_Violin_notch)+
                                    xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) 
                                } else
                                {
                                    p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                    geom_boxplot(aes(x = factor(eje_x1)),fill = settings_Global_fill,notch = settings_Box_Violin_notch)+
                                    xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) 
                                }
                        }
                    }


                    if (graphic_type_graphic_type=='violin')
                    {
                        val=fact_x
                        if (settings_Box_Violin_jitter=='TRUE')
                        {
                            if (settings_Global_fill_f=='Palette')
                            {
                                p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                geom_violin(aes(fill = val),notch = settings_Box_Violin_notch, trim=settings_Box_Violin_trim)+
                                xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) +  geom_jitter(size=settings_Global_size,colour =settings_Global_colour)
                            } else
                            {
                                p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                geom_violin(aes(x = factor(eje_x1)),fill = settings_Global_fill,notch = settings_Box_Violin_notch, trim=settings_Box_Violin_trim)+
                                xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) +  geom_jitter(size=settings_Global_size,colour =settings_Global_colour) 
                            }
                        
                        } else
                        {
                            if (settings_Global_fill_f=='Palette')
                                {
                                    
                                    p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                    geom_violin(aes(fill = val,label="BoxViolin"),notch = settings_Box_Violin_notch,outlier.colour =settings_Global_colour, trim=settings_Box_Violin_trim)+
                                    xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) 
                                } else
                                {
                                    p=ggplot(data = dataset, aes(x = factor(eje_x1), y = eje_y2)) + 
                                    geom_violin(aes(x = factor(eje_x1)),fill = settings_Global_fill,notch = settings_Box_Violin_notch,outlier.colour =settings_Global_colour, trim=settings_Box_Violin_trim)+
                                    xlab(n1) +    ylab(n2) +   ggtitle(graphic_type_graphic_type) 
                                }
                        }   
                        p=p+ theme_bw()
                    }
                    if (p!="")
                    {
                        if (settings_Global_Disposicion=='Horizontal')
                        {
                        p=p+coord_flip()  
                        }
                    }
                           shape_val=""
                        if (settings_Scatter_shape=='TRUE')
                        {      
                            if (exists("eje_z"))
                                {
                                    shape_val=eje_z3
                                }
                        }
                        
                        if (graphic_type_graphic_type=="scatter_point")
                        {
                            p=ggplot(dataset, aes(x=eje_x1, y=eje_y2)) + 
                                geom_point(size=settings_Global_size,colour =settings_Global_colour)+
                                geom_smooth(method=settings_Scatter_metodo)+
                                xlab(n1) +    ylab(n2) +
                                labs(
                                        title = paste(graphic_type_graphic_type ," -",  settings_Scatter_metodo)
                                    )
                        }

                        if (graphic_type_graphic_type=="Bar")
                        {
                            val=factor(eje_x1)
                            val=eje_x1
                            p=ggplot(dataset, aes(x=val)) + 
                                geom_bar(aes(fill = val),stat="count",colour =settings_Global_colour) + 
                                xlab(n1) +
                                labs(title = graphic_type_graphic_type)+ 
                                scale_fill_brewer(palette=settings_Global_fill)
                            if (settings_Global_Disposicion=='Horizontal')
                                    {
                                    p=p+coord_flip()  
                                    }
                        }


                        if (graphic_type_graphic_type=="Bar_2vars")
                        {
                            eje_y=eje_y2_f
                            eje_x=eje_x1
                            label_x=paste(n1 ," / ", n2)
                            p=ggplot(dataset, aes(eje_x,fill=eje_y)) + 
                                geom_bar(colour =settings_Global_colour) + 
                                xlab("eje_x") +   
                                labs(title = paste(graphic_type_graphic_type ," - ", label_x) ) +
                                scale_fill_brewer(palette=settings_Global_fill)                             
                            if (settings_Global_Disposicion=='Horizontal')
                                    {
                                    p=p+coord_flip()  
                                    }
                        }
                        if (graphic_type_graphic_type=="Bar_2vars(Separadas)")
                        {
                            eje_y=eje_y2_f
                            eje_x=eje_x1
                            label_x=paste(n1 ," / ", n2)
                            p=ggplot(dataset, aes(eje_x,fill=eje_y)) + 
                                geom_bar(colour =settings_Global_colour, position = "dodge") + 
                                xlab("eje_x") +   
                                labs(title = paste(graphic_type_graphic_type ," - ", label_x) ) +
                                scale_fill_brewer(palette=settings_Global_fill) 
                            if (settings_Global_Disposicion=='Horizontal')
                                    {
                                    p=p+coord_flip()  
                                    }       
                        }

                     if (graphic_type_graphic_type=="scatter_line")
                        {
                            p=ggplot(dataset, aes(x=eje_x1, y=eje_y2)) + 
                                geom_line(size=settings_Global_size,colour =settings_Global_colour)+
                                geom_smooth(method=settings_Scatter_metodo)+ 
                                xlab(n1) +    
                                ylab(n2) +
                                labs(
                                        title = paste(graphic_type_graphic_type ," -",  settings_Scatter_metodo)
                                    )
                                
                        }
                        if (graphic_type_graphic_type=="Histogram")
                        {
                            if (eje_x==eje_y)
                            {
                                val=factor(eje_x1)
                                val=eje_x1
                                p=ggplot(dataset, aes(x=val)) + 
                                    geom_histogram(fill="antiquewhite", color=settings_Global_colour) + 
                                    geom_freqpoly( colour=settings_Global_colour) +
                                    xlab(n1) +
                                    labs(title = graphic_type_graphic_type)+ 
                                    scale_fill_brewer(palette=settings_Global_fill)
                                if (settings_Global_Disposicion=='Horizontal')
                                    {
                                    p=p+coord_flip()  
                                    }       
                           }else
                            {
                                val=factor(eje_x1)
                                val_y=factor(eje_y2)
                                val=eje_x1
                                p=ggplot(dataset, aes(x=val, fill=val_y)) + 
                                    geom_histogram(color=settings_Global_colour) + 
                                    geom_freqpoly( color=settings_Global_colour) +
                                    xlab(n1) +
                                    ylab(n2) +
                                    labs(title = graphic_type_graphic_type)+ 
                                    scale_fill_brewer(palette=settings_Global_fill) 
                                if (settings_Global_Disposicion=='Horizontal')
                                    {
                                    p=p+coord_flip()  
                                    }       
                            }
                    
                        }

                    

                   
                ############# themes ###############
                if(settings_Global_themes=="theme_gray") { p=p+ theme_gray() }
                if(settings_Global_themes=="theme_linedraw") { p=p+ theme_linedraw() }
                if(settings_Global_themes=="theme_bw") { p=p+ theme_bw() }
                if(settings_Global_themes=="theme_dark") { p=p+ theme_dark() }
                if(settings_Global_themes=="theme_minimal") { p=p+ theme_minimal() }
                if(settings_Global_themes=="theme_classic") { p=p+ theme_classic() }
                if(settings_Global_themes=="theme_void") { p=p+ theme_void() }
                if(settings_Global_themes=="theme_test") { p=p+ theme_test() }
                
                if (graphic_type_graphic_type=='both')
                    {
                        fig =dataset %>%
                        plot_ly(
                            x= factor(eje_x1),
                            y = eje_y2,
                            split = factor(eje_x1),
                            type = 'violin',
                            box = list(
                            visible = T
                            ),
                            meanline = list(
                            visible = T
                            ),
                            x0 = ''
                        ) 

                        p =fig %>%
                        layout(
                            yaxis = list(
                            title = "BoxViolin",
                            zeroline = F
                            )
                        )
                    }

                if (graphic_type_graphic_type=='scatter3d mesh3d')
                    {
                        fig = plot_ly(dataset, x = eje_x1, y = eje_y2, z = eje_z3, xlab=n1,ylab=n2,zlab=n3, type = 'mesh3d',
                                marker = list( size=settings_Global_size, color =eje_z3,colorscale = settings_Global_fill, showscale = TRUE))
                         p = fig%>% add_markers(color =settings_Global_colour) %>%
                                        layout(
                                            scene = list(
                                            xaxis = list(title = n1),
                                            yaxis = list(title = n2),
                                            zaxis = list(title = n3)
                                            )
                                        )
                     }
                if (graphic_type_graphic_type=='scatter3d lines')
                    {
                        fig = plot_ly(dataset, x = eje_x1, y = eje_y2, z = eje_z3, xlab=n1,ylab=n2,zlab=n3, 
                                marker = list( size=settings_Global_size, color =eje_z3,colorscale = settings_Global_fill, showscale = TRUE))
                         p = fig%>% add_lines(color =settings_Global_colour) %>%
                                        layout(
                                            scene = list(
                                            xaxis = list(title = n1),
                                            yaxis = list(title = n2),
                                            zaxis = list(title = n3)
                                            )
                                        ) 
                     }      
                if (graphic_type_graphic_type=='scatter3d markers')
                    {
                        fig = plot_ly(dataset, x = eje_x1, y = eje_y2, z = eje_z3, xlab=n1,ylab=n2,zlab=n3,
                                marker = list( size=settings_Global_size, color =eje_z3,colorscale = settings_Global_fill, showscale = TRUE)) 
                         p = fig%>% add_markers(color =settings_Global_colour) %>%
                                        layout(
                                            scene = list(
                                            xaxis = list(title = n1),
                                            yaxis = list(title = n2),
                                            zaxis = list(title = n3)
                                            )
                                        ) 
                     }      


                               

             


                ############# Create and save widget ###############


                internalSaveWidget(p, 'out.html');

                ####################################################
    }# End STOP

}