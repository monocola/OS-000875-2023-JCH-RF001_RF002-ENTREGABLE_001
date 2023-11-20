import { Observable } from 'rxjs';
import { CabeceraMaestra } from '../../@data/model/cabeceraMaestra';
import {
  ConocimientoDataTratada
} from '../../@data/model/maestra/conocimiento';

export abstract class MaestraRepository {
  abstract getMaestraList(): Observable<CabeceraMaestra[]>;
  abstract getMaestraConocimiento(codtipo: number): Observable<ConocimientoDataTratada>;
  abstract getMaestraListDetail(
    idTablaMaestra?: number,
    entidadId?: number
  ): Observable<any[]>;
  abstract getMaestraDetalleByCod(codigo_cabecera: string): Observable<any[]>;
  abstract getComboSection(codigo_cabecera: string): Observable<any[]>;
  abstract getMaestraDetalleByCodandCodProg(
    codigo_cabecera: string,
    codProg: number
  ): Observable<any[]>;
  abstract filtrarMaestraDetalle(body: any): Observable<any[]>;

  abstract createOrUpdateMaestraDetalle(
    body: any,
    maeDetalleId?: number
  ): Observable<boolean>;
  abstract deleteMaestraDetalle(maeDetalleId: number): Observable<boolean>;
  abstract downloadBase64(url: string): Observable<boolean>;
}
