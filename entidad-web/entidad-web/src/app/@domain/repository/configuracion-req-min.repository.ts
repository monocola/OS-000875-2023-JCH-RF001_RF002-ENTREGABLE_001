import { Observable } from 'rxjs';

export abstract class ConfiguracionReqMinRepository {

  abstract getDeclaracionJuradaByPerfil(perfilId: number, baseId: number): Observable<any>;

  abstract getCarreras(perfilId: number, baseId: number): Observable<any>;

  abstract getEspecialidades(perfilId: number, baseId: number): Observable<any>;

  abstract getEspecificaciones(perfilId: number, baseId: number): Observable<any>;

  abstract getRequisitosAdicionales(perfilId: number, baseId: number): Observable<any>;

  abstract getExperiencialLaboral(perfilId: number, baseId: number): Observable<any>;

  abstract getPuntajesResumen(perfilId: number, regimenId: number, entidadId: number): Observable<any>;

  abstract getPesosConfiguracion(perfilId: number, baseId: number): Observable<any>;

  abstract setConfiguracionPerfil(payload: any): Observable<any>;

  abstract obtenerPlantillaPorPerfil(perfilId: number, baseId: number): Observable<any>;

  abstract getPlantillasConfiguracion(): Observable<any>;

  abstract getGradosCarreras(perfilId: number, baseId: number): Observable<any>;

  abstract setEliminarPerfil(perfilId: number, baseId: number): Observable<any>;

  abstract getListaInvestigacionYPublicacion(perfilId: number, baseId: number): Observable<any>;

}
