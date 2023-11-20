import { Observable } from 'rxjs';

export abstract class ResultadosPostulanteRepository {

  abstract listarFormacionBasicaSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarFormacionSuperiorSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarIdiomasSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarOfimaticaSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarEspecializacionSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarExperienciaSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract obtenerDatosPostulanteSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarOtrosRequisitosSeleccion (convocatoriaPostulanteId: any): Observable<any>;
  abstract listarDeclaraJuradasSeleccion (convocatoriaId: any, postulanteId: any): Observable<any>;
  abstract listarFormacionAcademica (idPerfil: any): Observable<any>;
  abstract descargarCV (convocatoriaPostulanteId: any): Observable<any>;

}
