import { Country } from 'src/app/@data/model/country';
import { EntityRequest } from 'src/app/@data/model/entityRequest';
import { Occupation } from 'src/app/@data/model/occupation';
import { Observable } from 'rxjs';
import { SolicitudEntidad } from '../../@data/model/solicitudesentidad';

export abstract class AdministratorRepository {
  abstract registerOrUpdateAdminRequest(
    registerForm: any,
    document: File,
    documentBase64: string,
    flag: number,
    fileChanged: boolean,
    entity?: EntityRequest
  ): Observable<any>;
  abstract getCountries(): Observable<Country[]>;
  abstract getOccupations(): Observable<Occupation[]>;
  abstract getSolicitudesEntidaes(
    estado: number,
    nivelGobierno: number,
    sector: number,
    razonSocial: string,
    ruc: string,
    fechaInicio: string,
    fechaFin: string,
    departamento: number,
    provincia: number,
    distrito: number
  ): Observable<SolicitudEntidad[]>;
  abstract getSolicitudById(id: string): Observable<EntityRequest>;
  abstract approveEntity(solicitudId: number): Observable<any>;
  abstract updateSolicitudPerson(
    solicitudId: number,
    entity: EntityRequest
  ): Observable<any>;
  abstract updateSolicitudPersonaJuridica(
    solicitudPersonId: number,
    ubigeoId: number,
    direccionCompleta: string,
    referencia: string
  ): Observable<any>;
  abstract observeRequest(
    solicitudId: number,
    arrayReason: number[]
  ): Observable<any>;
  abstract verifyCodeObservation(
    codeRequest: string,
    solicitudId: string
  ): Observable<any>;
}
