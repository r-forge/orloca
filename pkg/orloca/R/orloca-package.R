#' @docType package
#' @name orloca-package
#' @title Operations Research LOCational Analysis Models
#' 
#' @description
#' Objects and methods to handle and solve the min-sum location problem, also known as Fermat-Weber problem.
#' 
#' @details
#' \preformatted{
#' 
#' Package:   orloca
#' 
#' Type:      Package
#' 
#' Version:   5.6
#' 
#' Date:      2024-01-31
#' 
#' License:   GPL (>= 3)
#' }
#'
#' The min-sum location problem search for a point such that the weighted sum of the distances to the demand points are minimized. See "The Fermat-Weber location problem revisited" by Brimberg, Mathematical Programming, 1, pg. 71-76, 1995, \doi{10.1007/BF01592245}.
#'
#' General global optimization algorithms are used to solve the problem, along with the adhoc Weiszfeld method, see "Sur le point pour lequel la Somme des distances de n points donnes est minimum", by E. Weiszfeld, Tohoku Mathematical Journal, First Series, 43, pg. 355-386, 1937 or "On the point for which the sum of the distances to n given points is minimum", by E. Weiszfeld and F. Plastria, Annals of Operations Research, 167, pg. 7-41, 2009, \doi{10.1007/s10479-008-0352-z}.
#'
#' The package provides a class \code{loca.p} that represents a location problem with a finite set of demand points on the plane.
#' Also, it is possible to plot the points and the objective function.
#' Such objective function is the total weighted distances travelled by all the customers to the service.
#'
#'
#' Non-planar location problems could be handle in future versions of the package.
#'
#' 
#' For a demo, load the package with the instruction \code{library(orloca)}, and run the demo executing the instruction \code{demo(orloca)}.
#'
#' 
#' The package is ready for internationalization. The author kindly ask for translated version of the .mo file to include in the package.
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@@uca.es>
#' 
#' Mantainer: Manuel Munoz-Marquez <manuel.munoz@@uca.es>
#' @references
#' [1] Brimberg, J. \emph{The Fermat-Weber location problem revisited}, Mathematical Programming, 1, pg. 71-76, 1995. \doi{10.1007/BF01592245}.
#'
#' [2] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland. ISBN: 0-444-01031-9.
#'
#' [3] Weiszfeld, E. and Plastria, F. \emph{On the point for which the sum of the distances to n given points is minimum}, Annals of Operations Research, 167, pg. 7-41, 2009, \doi{10.1007/s10479-008-0352-z}.
#' 
#' [4] \url{http://knuth.uca.es/orloca/}
#' 
#' @keywords package optimize
#' @examples
#' # A new unweighted loca.p object
#' o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#' 
#' # Compute the sum of distances to point (3, 4)
#' # [1] 20.39384
#' distsum(o, 3, 4)
#' 
#' # Compute the sum of distances to point (3, 4) using lp norm with p = 2.5
#' # [1] 19.27258
#' distsum(o, 3, 4, lp = 2.5)
#'
#' # Solve the optimization problem
#' # [1] 0 0
#' distsummin(o)
#' 
#' # Contour plot
#' contour(o)
#'
#' # Run a demo of the package
#' demo(orloca)
#'
#' @import graphics
#' @import grDevices
#' @import knitr
#' @import methods
#' @import png
#' @import rmarkdown
#' @import stats
#' @import ucminf
#'
#' @export as.loca.p
#' @export as.loca.p.matrix
#' @export as.matrix.loca.p
#' @export contour.loca.p
#' @export loca.p
#' @export persp.loca.p
#' @export plot.loca.p
#' @export distsum
#' @export distsumgra
#' @export distsummin
#'
NULL

#' @rdname paquete-orloca
#' @name paquete-orloca
#' @title Modelos de Investigacion Operativa para el Analisis de Localizacion (Operations Research LOCational Analysis Models)
#'
#' @description
#' Objetos y metodos para manejar y resolver el problema de localizacion de suma ponderada minima, tambien conocido como problema de Fermat-Weber.
#'
#' \preformatted{
#' 
#' Paquete:   orloca
#' 
#' Version:   5.5
#' 
#' Fecha:      2023-09-19
#' 
#' Licencia:   GPL (>= 3)
#' }
#'
#' @details
#' El problema de localizacion de suma minima busca un punto tal que la suma ponderada de las distancias a los puntos de demanda se minimice.
#' Vease "The Fermat-Weber location problem revisited" por Brimberg, Mathematical Programming, 1, pag. 71-76, 1995. \doi{10.1007/BF01592245}.
#'
#' Se usan algoritmos generales de optimizacion global para resolver el problema, junto con el metodo adhoc Weiszfeld, vease "Sur le point pour lequel la Somme des distance de n points donnes est minimum", por Weiszfeld, Tohoku Mathematical Journal, First Series, 43, pag. 355-386, 1937 o "On the point for which the sum of the distances to n given points is minimum", por E. Weiszfeld y F. Plastria, Annals of Operations Research, 167, pg. 7-41, 2009. \doi{10.1007/s10479-008-0352-z}.
#'
#' El paquete proporciona una clase \code{loca.p} que representa un problema de localizacion sobre el plano.
#' Tambien permite dibujar los puntos junto a la funcion objetivo.
#' Dicho objetivo es la suma ponderada de las distancias que viajan los clientes del servicio.
#'
#' Versiones no planas del problema podrian incorporarse en futuras versiones del paquete.
#'
#' Para una demostracion, cargue el paquete con la instrucción \code{library(orloca)} y ejecute la demostracion con la instruccion \code{demo(orloca)}.
#' 
#' El paquete esta preparado para su internacionalizacion.
#' Las traducciones de los ficheros .mo recibidas seran anadidas en proximas versiones del paquete.
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' Mantenedor: Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @references
#' [1] Brimberg, J. \emph{The Fermat-Weber location problem revisited}, Mathematical Programming, 1, pg. 71-76, 1995. \doi{10.1007/BF01592245}.
#'
#' [2] Love, R. F., Morris, J. G., Wesolowsky, G. O. \emph{Facilities Location: Chapter 2: Introduction to Single-Facility Location}, 1988, North-Holland
#'
#' [3] Weiszfeld, E. and Plastria, F. \emph{On the point for which the sum of the distances to n given points is minimum}, Annals of Operations Research, 167, pg. 7-41, 2009, \doi{10.1007/s10479-008-0352-z}.
#'
#' [4] \url{http://knuth.uca.es/orloca/}
#'
#' @examples
#' # Un objeto loca.p no ponderado
#' o <- loca.p(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))
#'
#' # Calcula la funcion objetivo en el punto (3, 4)
#' # [1] 20.39384
#' zsum(o, 3, 4)
#'
#' # Calcula la suma de las distancias al punto (3, 4) usando la norma lp con p = 2.5
#' # [1] 19.27258
#' zsum(o, 3, 4, lp = 2.5)
#'
#' # Resuelve el problema de localizacion
#' # [1] 0 0
#' zsummin(o)
#'
#' # Curvas de nivel
#' contour(o)
#'
#' # Ejecuta una demo del paquete
#' demo(orloca)
NULL
