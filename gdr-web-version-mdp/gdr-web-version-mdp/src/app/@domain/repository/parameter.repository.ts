import { Sector } from './../../@data/model/sector';
import { Genre } from 'src/app/@data/model/genre';
import { TypeDocument } from 'src/app/@data/model/typeDocument';
import { Observable } from 'rxjs';
import { Ubigeo } from '../../@data/model/ubigeo';
import { RoleUser } from '../../@data/model/role';
import { ParameterItem } from 'src/app/@data/model/parameterItem';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';

export abstract class ParameterRepository {
  abstract getDepartamento(): Observable<Ubigeo[]>;
  abstract getProvincias(idDept: number): Observable<Ubigeo[]>;
  abstract getDistritos(idProv: number): Observable<Ubigeo[]>;
  abstract getEstadoSolicitud(): Observable<TypeDocument[]>;
  abstract getSector(): Observable<TypeDocument[]>;
  abstract getGobierno(): Observable<TypeDocument[]>;
  abstract getTypeDocuments(): Observable<TypeDocument[]>;
  abstract getTypeAgendamiento(): Observable<Genre[]>;
  abstract getZonaHorariaReunion(): Observable<Genre[]>;
  abstract getGenres(): Observable<Genre[]>;
  abstract getGovernmentSector(): Observable<any>;
  abstract getGovermentLevel(): Observable<any>;
  abstract getObservationReasons(): Observable<any>;
  abstract getEstadoRegistro(): Observable<TypeDocument[]>;
  abstract getRolesCuentas(): Observable<RoleUser[]>;
  abstract getRegistryStates(): Observable<ParameterItem[]>;
  abstract getOrganoNaturaleza(): Observable<ParameterItem[]>;
  abstract getTipoOrgano(): Observable<ParameterItem[]>;
  abstract getNivelesOrgano(): Observable<ParameterItem[]>;
  abstract getSectores(): Observable<MaestraParametro[]>;
  abstract getNiveles(): Observable<MaestraParametro[]>;
  abstract getTipoEntidad(): Observable<MaestraParametro[]>;
}
