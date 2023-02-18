
library("readxl") #Importar los datos
library("dplyr") # ManipulaciC3n de datos


usu_individual_T120_1_ <- read_excel("usu_individual_T120 (1).xlsx")

#Voy a renombrar el archivo para facilitar su manipulaciC3n.

Argentina <- usu_individual_T120_1_

Argentina_1 <- Argentina[, c(1, 4, 7,9, 11:21)] # armo nueva tabla con las columnas que quiero usar

# El archivo estC! codificado, voy a cambiar los nombre para que se entiendo mejor
#y sea mas facil leer los datos

colnames(Argentina_1)

Argentina_1 <- rename(Argentina_1,
                      c("provincia"=AGLOMERADO,
                        "parentesco"=CH03, 
                        "sexo"=CH04, 
                        "fecha_de_nacimiento"=CH05, 
                        "años_cumplidos"=CH06,
                        "estado_civil"=CH07, 
                        "cobertura_medica"=CH08,
                        "lee_y_escribe"=CH09,
                        "escolarizado"=CH10, 
                        "tipo_de_establecimiento"=CH11,
                        "nivel_de_cursado"=CH12,
                        "recibido"=CH13))


#voy a renombrar el archivo para que quede completo y con todos los datos sin codificar.

Argentina_1$REGION <- ifelse(Argentina_1$REGION == 1, "Gran Buenos Aires", 
                              ifelse(Argentina_1$REGION == 40, "NOA",
                                     ifelse(Argentina_1$REGION == 41, "NEA",
                                            ifelse(Argentina_1$REGION == 42, "Cuyo",
                                                   ifelse(Argentina_1$REGION == 43, "Pampeana",
                                                          ifelse(Argentina_1$REGION == 44, "Patagonia",NA))))))


Argentina_1$parentesco<- ifelse(Argentina_1$parentesco == 1, "jefe_a", 
                               ifelse(Argentina_1$parentesco == 2, "conyuge_pareja",
                                    ifelse(Argentina_1$parentesco == 3, "hijo_hijastro_a",
                                         ifelse(Argentina_1$parentesco == 4, "yerno_nuera",
                                                ifelse(Argentina_1$parentesco == 5, "nieto_a",
                                                       ifelse(Argentina_1$parentesco == 6, "madre_padre",
                                                              ifelse(Argentina_1$parentesco == 7,"suegro_a",
                                                                     ifelse(Argentina_1$parentesco == 8,"hermano_a",
                                                                           ifelse(Argentina_1$parentesco == 9,"otros_familiares",
                                                                                  ifelse(Argentina_1$parentesco == 10,"no_familiar",NA))))))))))



Argentina_1$sexo <- ifelse(Argentina_1$sexo == 1, "Varon", 
                              ifelse(Argentina_1$sexo == 2, "Mujer",NA))


Argentina_1$provincia<- ifelse(Argentina_1$provincia == 2, "Buenos Aires", 
                        ifelse(Argentina_1$provincia == 3, "Buenos Aires",
                        ifelse(Argentina_1$provincia == 4, "Santa Fe",
                        ifelse(Argentina_1$provincia == 5, "Santa Fe",
                        ifelse(Argentina_1$provincia ==6, "Entre Ríos",
                        ifelse(Argentina_1$provincia == 7, "Misiones",
                        ifelse(Argentina_1$provincia == 8,"Chaco",
                        ifelse(Argentina_1$provincia == 9,"Chubut",
                        ifelse(Argentina_1$provincia == 10,"Mendoza",
                        ifelse(Argentina_1$provincia == 12,"Corrientes",
                        ifelse(Argentina_1$provincia==13, "Córdoba",
                        ifelse(Argentina_1$provincia==14, "Entre Ríos",
                        ifelse(Argentina_1$provincia==15, "Formosa",
                        ifelse(Argentina_1$provincia==17, "Neuquén",
                        ifelse(Argentina_1$provincia==18, "Santiago del Estero",
                        ifelse(Argentina_1$provincia==19, "Jujuy",
                        ifelse(Argentina_1$provincia==20, "Santa Cruz",
                        ifelse(Argentina_1$provincia==22, "Catamarca",
                        ifelse(Argentina_1$provincia==23, "Salta",
                        ifelse(Argentina_1$provincia==25, "La Rioja",
                        ifelse(Argentina_1$provincia==26, "San Luis",
                        ifelse(Argentina_1$provincia==27, "San Juan",
                        ifelse(Argentina_1$provincia==29, "Tucumán",
                        ifelse(Argentina_1$provincia==30, "La Pampa",
                        ifelse(Argentina_1$provincia==31, "Tierra del Fuego",
                        ifelse(Argentina_1$provincia==32, "Buenos Aires",
                        ifelse(Argentina_1$provincia==33, "Buenos Aires",
                        ifelse(Argentina_1$provincia==34, "Buenos Aires",
                        ifelse(Argentina_1$provincia==36, "Córdoba",
                        ifelse(Argentina_1$provincia==38, "Santa Fe",
                        ifelse(Argentina_1$provincia==91, "Chubut",
                        ifelse(Argentina_1$provincia==93, "Río Negro",
                        NA))))))))))))))))))))))))))))))))



Argentina_1$estado_civil <- ifelse(Argentina_1$estado_civil == 1, "unido", 
                              ifelse(Argentina_1$estado_civil == 2, "casado",
                                     ifelse(Argentina_1$estado_civil == 3, "separado_a_o_divorciado_a",
                                            ifelse(Argentina_1$estado_civil == 4, "viudo_a",
                                                   ifelse(Argentina_1$estado_civil == 5, "soltero_a",NA)))))

Argentina_1$cobertura_medica <- ifelse(Argentina_1$cobertura_medica == 1, "obra_social", 
                              ifelse(Argentina_1$cobertura_medica == 2, "mutual_prepaga",
                                     ifelse(Argentina_1$cobertura_medica == 3, "planes_y_seguros_publicos",
                                            ifelse(Argentina_1$cobertura_medica == 4, "No_paga_ni_le_descuentan",
                                                   ifelse(Argentina_1$cobertura_medica== 9, "ns_nr",
                                                          ifelse(Argentina_1$cobertura_medica == 12, "obra_social_y_mutual",
                                                                 ifelse(Argentina_1$cobertura_medica == 13, "obra_social_y_planes_y_seguros",
                                                                      ifelse(Argentina_1$cobertura_medica == 123, "todo",NA))))))))


Argentina_1$lee_y_escribe <- ifelse(Argentina_1$lee_y_escribe == 1, "si", 
                              ifelse(Argentina_1$lee_y_escribe == 2, "no",
                                     ifelse(Argentina_1$lee_y_escribe == 3, "menor de dos aC1os",NA)))

Argentina_1$escolarizado <- ifelse(Argentina_1$escolarizado == 1, "si asiste", 
                              ifelse(Argentina_1$escolarizado == 2, "no asiste pero asistio",
                                     ifelse(Argentina_1$escolarizado == 3, "nunca asistio",NA)))

Argentina_1$tipo_de_establecimiento <- ifelse(Argentina_1$tipo_de_establecimiento == 1, "publico", 
                              ifelse(Argentina_1$tipo_de_establecimiento == 2, "privado",
                                     ifelse(Argentina_1$tipo_de_establecimiento == 9, "ns/nr",NA)))

Argentina_1$nivel_de_cursado <- ifelse(Argentina_1$nivel_de_cursado == 1, "Jardin o prescolar", 
                              ifelse(Argentina_1$nivel_de_cursado == 2, "primario",
                                     ifelse(Argentina_1$nivel_de_cursado == 3, "EGB",
                                            ifelse(Argentina_1$nivel_de_cursado == 4, "secundario",
                                                   ifelse(Argentina_1$nivel_de_cursado == 5, "polimodal",
                                                          ifelse(Argentina_1$nivel_de_cursado == 6, "terciario",
                                                                 ifelse(Argentina_1$nivel_de_cursado == 7, "universitario posgrado",
                                                                        ifelse(Argentina_1$nivel_de_cursado == 8, "universitario",
                                                                               ifelse(Argentina_1$nivel_de_cursado == 9, "educacion especial",NA)))))))))


Argentina_1$recibido <- ifelse(Argentina_1$recibido== 1, "si", 
                              ifelse(Argentina_1$recibido == 2, "no",
                                     ifelse(Argentina_1$recibido == 9, "ns/nr",NA)))

#para guardar en el directorio que estoy trabajando los datoas limpios

write.csv(Argentina_1, file = "Argentina_limpio.xlsx", row.names = TRUE) 

