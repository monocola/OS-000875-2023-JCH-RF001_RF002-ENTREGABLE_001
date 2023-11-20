import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { Observable } from 'rxjs';
import { map, timeout } from 'rxjs/operators';
import { ResponseRequest } from '../model/reponse-request';
import { Const } from './const';
import { requestFilter } from '../../utils/general';
import { MaestraParametro } from '../model/maestra-parametro';
import { DatosPersonalesServidorCivil } from '../model/servidores-civiles';
import { PuestoUoServidorCivil } from '../model/puesto';
import { DatePipe } from '@angular/common';

@Injectable({
  providedIn: 'root',
})
export class ServidoresService implements ServidoresRepository {
  constructor(
    private http: HttpClient,
    private datePipe: DatePipe,
    private authService: AuthenticationRepository
  ) {}

  uploadFileMasivo(body: any): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const user = this.authService.getCurrentUserValue.numeroDocumento;
    const url = `${Const.API_ENTIDAD}v1/registrar/procesoAsincrono`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        tipoProcesoId: 1,
        usuarioEnvio: user,
        fechaEnvio: new Date(),
        nombreArchivo: "srvcv",
        extensionArchivo: ".xlsm",
        jsonVariablesInput: "{\"entidadId\": "+entidadId+"}",
        base64File: body.split('base64,')[1],
        //entidadId: entidadId,
      },
      
    };
    return this.http.post(url, tramaEnvio).pipe(
      timeout(12000000),
      map((res: ResponseRequest) => {
        if (res.status.success) {
            return res;
        } else {
          throw new Error(res.status.error.messages[0]).message;
        }
      })
    );
  }

  downloadExcel() {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/downloadFormatServCivil?idEntidad=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  getOrganigramas(): Observable<any[]> {
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;
    const url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaServidorCivil;
        }
      })
    );
  }

  searchOrganigramaFilter(body: any): Observable<any[]> {
    let url;
    // const entidadId = this.authService.getCurrentUserValue.entidadId;
    const entidadId = JSON.parse(sessionStorage.getItem('entidad')).entidadId;

    const filterString = requestFilter(body);
    if (filterString.length > 1) {
      url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}&${filterString}`;
    } else {
      url = `${Const.API_ENTIDAD}v1/organigrama/servidores/civiles?entidadId=${entidadId}`;
    }
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaServidorCivil;
      })
    );
  }

  getTiposDocumento(): Observable<MaestraParametro[]> {
    const url = `${Const.API_ENTIDAD}v1/maestra?tipoParametro=PER_TIPO_DOCUMENTO`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.items;
        }
      })
    );
  }

  eliminarServidor(personaId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/${personaId}?estado=0`;
    return this.http.delete(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  getDatosPersonales(
    detalleUoId: number,
    personaId: number
  ): Observable<DatosPersonalesServidorCivil> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/datosPersonales?entidadId=${entidadId}&detuoId=${detalleUoId}&personaId=${personaId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.datosPersonalesServidorCivil;
        }
      })
    );
  }

  agregarServidor(body: any): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/crear`;
    return this.http.post(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  actualizarServidor(datos: DatosPersonalesServidorCivil): Observable<any> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/editar`;
    const body = {
      payload: {
        entidadId: entidadId,
        detuoId: datos.detalleuoId,
        personaId: datos.personaId,
        telefono: datos.telefono,
        correoInstitucional: datos.correoInstitucional,
        correoAlterno: datos.correoAlternativo,
        sindicatoFlag: datos.sindicato,
        regimenLaboralId: datos.regimenLaboralId,
      },
    };
    return this.http.put(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  agregarPuesto(datos: PuestoUoServidorCivil): Observable<any> {
    datos.entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/agregarPuesto`;
    const body = { payload: datos };

    return this.http.put(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }
 
  editarPuesto(datos: PuestoUoServidorCivil): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/actualizarDetallePuesto`;
    const payload = {
      detuoId: datos.detuoId,
      entidadId: this.authService.getCurrentUserValue.entidadId,
      fechaCese: datos.fechaCese,
      fechaInicio: datos.fechaInicio,
      strFechaInicio: this.datePipe.transform(
        datos.fechaInicio,
        'yyyy-MM-dd HH:mm:ss'
      ),
      // personaIdAsignada: datos.personaIdAsignada,
      puestoId: datos.puestoId,
      nombrePuesto: datos.puesto,
      tipoAsignacion: datos.tipoAsignacion,
      uoId: datos.uoId,
      motivoId: datos.motivoId,
    };
    const body = { payload: payload };

    return this.http.put(url, body).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload;
        }
      })
    );
  }

  getHisotrialPuestos(
    personaId: number,
    uoId: number
  ): Observable<PuestoUoServidorCivil[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/puestoUO?entidadId=${entidadId}&personaId=${personaId}&uoId=${uoId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaPuestoUoServidorCivil;
        }
      })
    );
  }

  listarPersonasParaPuesto(uoId: number, puestoId: number): Observable<any[]> {
    const entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/personasPuesto?entidadId=${entidadId}&uoId=${uoId}&puestoId=${puestoId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (!response.status.success) {
          throw new Error(response.status.error.messages[0]).message;
        } else {
          return response.payload.listaPersonasPuestoUoServidorCivil;
        }
      })
    );
  }

  getDatosPersonalesRegimen(detalleUoId: number, personaId: number, regimenId: number): Observable<DatosPersonalesServidorCivil> {
    let entidadId = this.authService.getCurrentUserValue.entidadId;
    const url = `${Const.API_ENTIDAD}v1/servidorCivil/datosPersonales?entidadId=${entidadId}&detuoId=${detalleUoId}&personaId=${personaId}&regimenId=${regimenId}`;
    return this.http.get(url).pipe(
      map(
        (response: ResponseRequest) => {
          if (!response.status.success) {
            throw new Error(response.status.error.messages[0]).message;
          } else {
            return response.payload.datosPersonalesServidorCivil;
          }
        }
      )
    );
  }

}

