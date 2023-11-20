import { Observable } from 'rxjs';

export abstract class MaestraEntidadRepository {
  abstract asignaMaestraDetalle(
    cabeceraId: number,
    detalleId: number
  ): Observable<any[]>;
  abstract asignaMaestraDetalleEntidad(
    cabeceraId: number,
    detalleId: number,
    entidadId: number
  ): Observable<any[]>;
  abstract actualizaConfigMaestraDetalle(
    configMaestraId: number,
    estado: boolean
  ): Observable<any[]>;
  abstract getMaestraDetalleEntidad(cabeceraId?: number): Observable<any[]>;
  abstract getMaeDetalleEntByCod(codProg?: string): Observable<any>;
  abstract getCamposAsignadosByCodCabecera(
    COD_CABECERA?: string
  ): Observable<any>;
  abstract createOrUpdateMaestraDetalle(
    body: any,
    maeCabeceraId: number,
    maeDetalleEntidadId?: boolean
  ): Observable<boolean>;
  abstract getRegimenModalidad(regimen:number, modalidad:number): Observable<any>;
}
