import { Observable } from 'rxjs';

export abstract class PerfilesRepository {
  abstract getPerfiles(body?: any): Observable<any[]>;

  abstract getIdentificacion(perfilId): Observable<any>;
  abstract saveOrUpdateIdentificacion(
    body,
    regimenId,
    perfilIdentificacionId?
  ): Observable<any>;

  abstract getFuncionesData(perfilId): Observable<any>;
  abstract saveOrUpdateFunciones(
    body,
    regimenId,
    perfilId,
    perfilFuncionId?
  ): Observable<any>;

  abstract getFormacionData(perfilId): Observable<any>;
  abstract saveOrUpdateFormacion(body, regimenId, perfilId): Observable<any>;

  abstract getExperienceData(perfilId): Observable<any>;
  abstract saveOrUpdateExperiencia(
    body,
    perfilId,
    perfilExperienciaId?
  ): Observable<any>;

  abstract getListaPerfilGrupo(): Observable<any[]>;
  abstract inactivatePerfil(perfilId): Observable<any>;

  abstract getRegimenesToCreate(): Observable<any[]>;
  abstract getPerfilGrupo(id?: number): Observable<any[]>;

  abstract getConocimientosByType(tipoId: number): Observable<any>;
  abstract getCarrerasBySituation(situationId: number): Observable<any>;
}
