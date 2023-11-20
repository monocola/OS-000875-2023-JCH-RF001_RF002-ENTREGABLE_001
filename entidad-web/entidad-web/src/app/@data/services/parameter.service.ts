import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Genre } from '../model/genre';
import { TypeDocument } from '../model/typeDocument';
import { Const } from './const';
import { Ubigeo } from '../model/ubigeo';
import { RoleUser } from '../model/role';

@Injectable({
  providedIn: 'root',
})
export class ParameterService implements ParameterRepository {
  constructor(private http: HttpClient) {}

  getDepartamento(): Observable<Ubigeo[]> {
    const url = `${Const.API_MAESTRA}v1/ubigeos/sunat/departamentos/`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getDepartamentoReniec(): Observable<Ubigeo[]> {
    const url = `${Const.API_MAESTRA}v1/ubigeos/reniec/departamentos/`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getProvincias(idDept: number): Observable<Ubigeo[]> {
    const url = `${Const.API_MAESTRA}v1/ubigeos/sunat/departamentos/${idDept}/provincias`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getDistritos(idProv: number): Observable<Ubigeo[]> {
    const url = `${Const.API_MAESTRA}v1/ubigeos/sunat/provincias/${idProv}/distritos`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getEstadoSolicitud(): Observable<TypeDocument[]> {
    const parametro = 'ENT_ESTADO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getSector(): Observable<TypeDocument[]> {
    const parametro = 'ENT_SECTOR';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getEstadoRegistro(): Observable<TypeDocument[]> {
    const parametro = 'ESTADO_REGISTRO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getRolesCuentas(): Observable<RoleUser[]> {
    const aplicacionId = Const.APPLICATION_ID;
    const url = `${Const.API_SEGURIDAD}v1/aplicaciones/${aplicacionId}/roles`;
    return this.http.get<any>(url).pipe(
      map((response) => {
        return response.payload.items.filter(
          (item) =>
            item.rolId !== Const.R_ADMIN_SERVIR &&
            item.rolId !== Const.R_POSTULANTE
        );
      })
    );
  }

  getGobierno(): Observable<TypeDocument[]> {
    const parametro = 'ENT_NIVEL_GOBIERNO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getTypeDocuments(): Observable<TypeDocument[]> {
    const parametro = 'PER_TIPO_DOCUMENTO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items.filter(
          (type) => type.codigoNumero === 1 || type.codigoNumero === 4
        );
      })
    );
  }

  getGenres(): Observable<Genre[]> {
    const parametro = 'PER_SEXO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getGovernmentSector(): Observable<any> {
    const parametro = 'ENT_SECTOR';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getGovermentLevel(): Observable<any> {
    const parametro = 'ENT_NIVEL_GOBIERNO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getObservationReasons(): Observable<any> {
    const parametro = 'OBSERV_ENTIDAD';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getRegistryStates(): Observable<any> {
    const parametro = 'ESTADO_REGISTRO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getOrganoNaturaleza(): Observable<any> {
    const parametro = 'TIPO_NATURALEZA';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getTipoOrgano(): Observable<any> {
    const parametro = 'TIPO_ORGANO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getNivelesOrgano(): Observable<any> {
    const parametro = 'NIVEL_ORGANO';
    const url = `${Const.API_MAESTRA}v1/tiposparametro/${parametro}/parametros`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

}
