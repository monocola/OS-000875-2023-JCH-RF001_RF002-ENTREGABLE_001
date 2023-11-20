import { Observable } from "rxjs";
import { MaestraParametro } from "src/app/@data/model/maestra-parametro";
import { PayloadMetas } from "src/app/@data/model/meta";
import { IParticipanteEvaluador } from "src/app/@data/model/participante";
import { PuestoUoServidorCivil } from "src/app/@data/model/puesto";
import { DatosPersonalesServidorCivil } from "src/app/@data/model/servidores-civiles";
import { Status } from '../../@data/model/reponse-request';

export abstract class ServidoresRepository {

  abstract uploadFileMasivo(body: any, cicloId: any): Observable<any>;
  abstract downloadExcel(): Observable<string>;
  abstract getOrganigramas(): Observable<any[]>;
  abstract searchOrganigramaFilter(body: any): Observable<any[]>;
  abstract getTiposDocumento(): Observable<MaestraParametro[]>;

  abstract eliminarServidor(personaId: number): Observable<any>;
  abstract getDatosPersonales(personaId: number): Observable<DatosPersonalesServidorCivil>;
  abstract agregarServidor(datos: DatosPersonalesServidorCivil): Observable<any>;
  abstract actualizarServidor(datos: DatosPersonalesServidorCivil): Observable<any>;
  abstract agregarPuesto(datos: PuestoUoServidorCivil): Observable<any>;
  abstract editarPuesto(datos: PuestoUoServidorCivil): Observable<any>;
  abstract getHisotrialPuestos(personaId: number, uoId: number): Observable<PuestoUoServidorCivil[]>;
  abstract listarPersonasParaPuesto(uoId: number, tipoAsignacion: string): Observable<any[]>;
  abstract listarParticipantes(body: any): Observable<IParticipanteEvaluador[]>;
  abstract listarParticipantesServidoresCiviles(body: any): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluadores(): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluadoresentidad(): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluados(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluadosEntidadSub(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluadosPersona(personaId: number): Observable<IParticipanteEvaluador[]>;
  abstract updateSegmento(body: any): Observable<any>;
  abstract activarParticipante(body: any): Observable<any>;
  abstract listarActividades(cronogramaId: number, etapaId: number): Observable<any[]>;
  abstract listarRolesPorSegmento(segmentoId: number, flagJefeUo: string): Observable<any[]>;

  abstract listarSinEvaluador(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract listarEvaluadosSinEvaluador(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract listarSinMandoMedio(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract listarMandosMedio(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;
  abstract eliminarEvaluado(evaluador: IParticipanteEvaluador): Observable<Status>;
  abstract agregarMandoMedio(mandoMedio: IParticipanteEvaluador, evaluados: IParticipanteEvaluador[]): Observable<Status>;
  abstract agregarMandoMedioEvaluado(mandoMedio: IParticipanteEvaluador, evaluados: IParticipanteEvaluador[]): Observable<Status>;
  
  abstract listarMetasParticipante(personaId: number, detUoId: number, cicloId: number): Observable<PayloadMetas>;

  abstract validarMeta(body: any): Observable<any>;
  abstract observarMeta(body: any): Observable<any>;
  abstract listarEvaluadosSinEvaluadorEntidad(evaluador: IParticipanteEvaluador): Observable<IParticipanteEvaluador[]>;












 // abstract agregarEvidencia(body: any): Observable<any>;




}
