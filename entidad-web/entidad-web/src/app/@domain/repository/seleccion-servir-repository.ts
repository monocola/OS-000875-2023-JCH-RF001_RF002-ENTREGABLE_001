import {
  Convocatoria,
  ListaConocimiento,
  Regimen,
  ResumenRespuestas,
  RespDatoConvocatoria,
} from './../../@data/model/lista-evaluaciones/entity';
import { Observable } from 'rxjs';
import { ExamenCorregidoResumen } from 'src/app/@data/model/examenCorregido';

export abstract class SeleccionServirRepository {
  abstract getConvocatoriasEntidad(
    entidadId: number
  ): Observable<Convocatoria[]>;

  abstract getRegimenEntidad(entidadId: number): Observable<Regimen[]>;

  abstract getListaConocimientos(request: any): Observable<ListaConocimiento[]>;

  abstract listarExamenCorregido(
    cabeceraId: number
  ): Observable<ExamenCorregidoResumen>;

  abstract getCapturaImagenPostulante(
    examenId: number,
    postulanteId: number
  ): Observable<any>;

  abstract getPreguntasCorregidas(
    cabeceraId: number
  ): Observable<ResumenRespuestas>;

  abstract getDetalleExamen(
    cabeceraId: number
  ): Observable<ResumenRespuestas>;

  abstract getDatosConvocatoriaById(
    convocatoriaId: number
  ): Observable<RespDatoConvocatoria>;

  abstract getTipoContrato(regimen: number): Observable<RespDatoConvocatoria>;

  abstract getPostulantesByConvocatoria(
    baseId: number,
    perfilId: number,
    estadoId: number,
    rncss: number,
    reqmin: number,
    fechaIni: string,
    fechaFin: string,
    page: number,
    size: number
  ): Observable<any>;

  abstract GetPostulanteEtapaReclutamiento(
    baseId: number,
    perfilId: number,
    estadoId: number,
    rncss: number,
    reqmin: number,
    fechaIni: string,
    fechaFin: string,
    page: number,
    size: number
  ): Observable<any>;

  abstract EtapaProgresoReclutamiento(
    baseId: number,
    etapaId: number
  ): Observable<any>;

  abstract updateFlagRedam(request: any): Observable<any>;

  abstract FiltroEleccion(
    baseId: number,
    valor: string,
    page: number,
    size: number
  ): Observable<any>;

  abstract guardarPregAbiertas (cabeceraId: number, request: any): Observable<any>;

  abstract getInterpolacion(entidadId: number, baseId: number): Observable<any>;
}
