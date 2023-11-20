import { Injectable } from '@angular/core';
import { map } from 'rxjs/operators';
import { SolicitudExternaRepository } from '../../@domain/repository/solicitud.externa.repository';
import { HttpClient, HttpRequest } from '@angular/common/http';
import { Observable } from 'rxjs/Observable';
import { Const } from './const';
import { ResponseRequest } from '../model/reponse-request';
import { SolicitudExterna } from '../model/SolicitudExterna';

@Injectable({
  providedIn: 'root'
})
export class SolicitudExternaService implements SolicitudExternaRepository {

  solicitud: SolicitudExterna;
  constructor(
    private http: HttpClient,
  ) {}

  actualizarSolicitudExterna(request: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        rucEntidad: request.rucEntidad,
        sindicato: request.sindicato,
        razonSocial: request.razonSocial,
        abreviatura: request.abreviatura,
        nombreEntidad: request.nombreEntidad,
        nivelGobiernoId: request.nivelGobiernoId,
        nivelGobierno: request.nivelGobierno,
        sectorId: request.sectorId,
        sector: request.sector,
        tipoEntidadId: request.tipoEntidadId,
        tipoEntidad: request.tipoEntidad,
        tipoDocumento: request.tipoDocumento,
        numeroDocumento: request.numeroDocumento,
        apellidoPaterno: request.apellidoPaterno,
        apellidoMaterno: request.apellidoMaterno,
        nombres: request.nombres,
        fechaNacimiento: request.fechaNacimiento,
        telefonoFijo: request.telefonoFijo,
        anexo: request.anexo,
        celular: request.celular,
        correoElectronico: request.correoElectronico,
        correoElectronicoGestor: request.correoElectronicoGestor,
        estadoSolicitud: request.estadoSolicitud,
        uuId: request.uuId,
        base64Image: request.base64Image,
        solicitudExtId: request.solicitudExtId
      },
    };
    const url = `${Const.API_ENTIDAD}v1/actualizar/solicitudExterna`;
    return this.http.put(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

  enviarSolicitudExterna(request: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        rucEntidad: request.rucEntidad,
        sindicato: request.sindicato,
        razonSocial: request.razonSocial,
        abreviatura: request.abreviatura,
        nombreEntidad: request.nombreEntidad,
        nivelGobiernoId: request.nivelGobiernoId,
        nivelGobierno: request.nivelGobierno,
        sectorId: request.sectorId,
        sector: request.sector,
        tipoEntidadId: request.tipoEntidadId,
        tipoEntidad: request.tipoEntidad,
        tipoDocumento: request.tipoDocumento,
        numeroDocumento: request.numeroDocumento,
        apellidoPaterno: request.apellidoPaterno,
        apellidoMaterno: request.apellidoMaterno,
        nombres: request.nombres,
        fechaNacimiento: request.fechaNacimiento,
        telefonoFijo: request.telefonoFijo,
        anexo: request.anexo,
        celular: request.celular,
        correoElectronico: request.correoElectronico,
        correoElectronicoGestor: request.correoElectronicoGestor,
        estadoSolicitud: request.estadoSolicitud,
        uuId: request.uuId,
        base64Image: request.base64Image
      },
    };
    const url = `${Const.API_ENTIDAD}v1/registrar/solicitudExterna`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

  subirDocumentoSolicitudExterna(body: any, archivo: any): Observable<any> {

    const paramFD: FormData = new FormData();
    paramFD.append('archivo', archivo);
    paramFD.append('extensiones', body.extensiones);
    paramFD.append('ruc', body.ruc);
    paramFD.append('ruta', body.ruta);

    const url = `${Const.API_PLANIFICACION}v1/uploadFile/alfresco`;

    const req = new HttpRequest(
      'POST',
      url,
      paramFD,
      {
        reportProgress: true,
        responseType: 'json',
      }
    );
    console.log(req);
    return this.http.request(req);
  }

  getAnio(): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/externa/anio/solicitudExt`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload;
      })
    );
  }

  getSolicitudExt(body: any): Observable<any[]> {
    let url = null;
    if ( body.anio ) { url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/filtrar?anio=${body.anio}`; }
    if ( body.estado ) { url = url +  `&estadoSolicitud=${body.estado}`; }
    if ( body.rucEntidad ) { url = url +  `&rucEntidad=${body.rucEntidad}`; }
    if ( body.numeroDocumento ) { url = url +  `&numeroDocumento=${body.numeroDocumento}`; }
    if ( body.razonSocial ) { url = url +  `&razonSocial=${body.razonSocial}`; }
    if ( body.nombreCompleto ) { url = url +  `&nombreCompleto=${body.nombreCompleto}`; }
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.solicitudExternaDto.map(item => {
          item.colorEstado = '<span class="' + item.estado.toLowerCase() +
            '"><span class="dot"></span>&nbsp&nbsp' + item.estado.toUpperCase() + '</span>';
          return item;
        });
      })
    );
  }

  getSolicitudExtId(Id: number): Observable<any> {
    let url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/${Id}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        // return response.payload;
        response.payload.colorEstado = '<span class="' + response.payload.estado.toLowerCase() +
          '"><span class="dot"></span>&nbsp&nbsp' + response.payload.estado.toUpperCase() + '</span>';
        return response.payload;
      })
    );
  }

  rechazaSolicitudExt(id: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        solicitudExtId: id
      },
    };
    const url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/rechazar`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

  observaSolicitudExt(id: number, obs: any): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        solicitudExtId: id,
        solicitudObs: obs
      },
    };
    const url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/observar`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

  validaSolicitudExt(Id: number): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        solicitudExtId: Id
      },
    };
    const url = `${Const.API_ENTIDAD}v1/externa/solicitudExt/validar`;
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        console.info(response);
        return response;
      })
    );
  }

}
