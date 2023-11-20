import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Inject, Injectable, LOCALE_ID } from '@angular/core';
import { AdministratorRepository } from 'src/app/@domain/repository/administrator.repository';
import { from, Observable } from 'rxjs';
import { catchError, map, mergeMap, timeout, toArray } from 'rxjs/operators';
import { Occupation } from '../model/occupation';
import { Const } from './const';
import { SolicitudEntidad } from '../model/solicitudesentidad';
import { Utils } from '../../utils/utils';

import { EntityAdapter, EntityRequest } from '../model/entityRequest';
import { getMiliseconds } from 'src/app/utils/general';
import { Country } from '../model/country';
import moment from 'moment';

@Injectable({
  providedIn: 'root',
})
export class AdministratorService implements AdministratorRepository {
  headers = new HttpHeaders().set('Content-Type', 'application/json');

  constructor(
    private http: HttpClient,
    @Inject(LOCALE_ID) private locale: string,
    private entityAdapter: EntityAdapter
  ) {}

  getCountries(): Observable<Country[]> {
    const url = `${Const.API_MAESTRA}v1/pais/query`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.items;
      })
    );
  }

  getOccupations(): Observable<Occupation[]> {
    const url = `${Const.API_ENTIDAD}v1/cargo`;
    return this.http.get(url).pipe(
      map((response: any) => {
        return response.payload.listaCargo;
      })
    );
  }

  getSolicitudesEntidaes(
    estado = 0,
    nivelGobierno = 0,
    sector = 0,
    razonSocial = null,
    ruc = null,
    fechaInicio = null,
    fechaFin = null,
    departamento = null,
    provincia = null,
    distrito = null
  ): Observable<SolicitudEntidad[]> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/filter?authorizationHeader=asdas`;
    let params = {
      estado: estado == null ? '0' : estado.toString(),
      nivelGobierno: nivelGobierno == null ? '0' : nivelGobierno.toString(),
      sector: sector == null ? '0' : sector.toString(),
    };
    if (razonSocial != null && razonSocial.length !== 0)
      params['razonSocial'] = razonSocial.toString();
    if (ruc != null && ruc.length !== 0) params['ruc'] = ruc.toString();
    if (fechaInicio != null && fechaInicio.length !== 0)
      params['fechaInicio'] = Utils.parseDate(fechaInicio, 'YYYY/MM/DD');
    if (fechaFin != null && fechaFin.length !== 0)
      params['fechaFin'] = Utils.parseDate(fechaFin, 'YYYY/MM/DD');
    if (departamento != null && departamento.length !== 0)
      params['departamento'] = departamento.toString();
    if (provincia != null && provincia.length !== 0)
      params['provincia'] = provincia.toString();
    if (distrito != null && distrito.length !== 0)
      params['distrito'] = distrito.toString();
    return this.http
      .get(url, { params: params })
      .pipe(map((response: any) => response.payload.listaObtenerSolicitud))
      .pipe(mergeMap((value) => from(value)))
      .pipe(
        map((response: any) => {
          response.lugarDep = `${
            response.departamento === null ? '-' : response.departamento
          }/${response.provincia === null ? '-' : response.provincia}/${
            response.distrito === null ? '-' : response.distrito
          }`;
          response.nombreCompleto = `${
            response.nombres === null ? '' : response.nombres
          } ${
            response.apellidoPaterno === null
              ? ''
              : ' ' + response.apellidoPaterno
          }${
            response.apellidoMaterno === null
              ? ''
              : ' ' + response.apellidoMaterno
          }`;
          if (response.fechaRegistro)
            response.fechaRegistro = Utils.parseDate(
              response.fechaRegistro,
              'DD/MM/yyyy'
            );
          // if (response.fechaAlta)
          //   response.fechaAlta = Utils.parseDate(response.fechaAlta, 'DD/MM/yyyy');
          // if (response.fechaBaja)
          //   response.fechaBaja = Utils.parseDate(response.fechaBaja, 'DD/MM/yyyy');
          return response;
        })
      )
      .pipe(toArray());
  }

  getSolicitudById(id: string): Observable<EntityRequest> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/${id}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.payload.solicitudEntidad) {
          return this.entityAdapter.adapt(response.payload);
        } else {
          throw new Error('No existe la solicitud ingresada').message;
        }
      }),
      timeout(3000),
      catchError((error) => {
        throw new Error('Tiempo excedido').message;
      })
    );
  }

  approveEntity(solicitudId: number): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/darAlta/${solicitudId}`;
    return this.http.put(url, null, { headers: this.headers }).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  verifyCodeObservation(
    codeRequest: string,
    solicitudId: string
  ): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/${codeRequest}/${solicitudId}`;
    return this.http.get(url).pipe(
      map((response: any) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  observeRequest(solicitudId: number, arrayReasons: number[]): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/observacion/${solicitudId}`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        listaIdObservacion: arrayReasons,
      },
    };
    return this.http.put(url, tramaEnvio, { headers: this.headers }).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateSolicitudPerson(
    solicitudId: number,
    entity: EntityRequest
  ): Observable<any> {
    const administrador = Object.assign({}, entity.personaNatural);
    const solicitud = Object.assign({}, entity.solicitudEntidad);
    solicitud.estadoSolicitud = 40;
    administrador.fechaNacimiento = moment(
      administrador.fechaNacimiento,
      'DD/MM/YYYY'
    ).toDate();
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/${solicitudId}`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        solicitud,
        listaSolicitudPersona: [administrador],
      },
    };
    return this.http.put(url, tramaEnvio, { headers: this.headers }).pipe(
      map((response: any) => {
        if (response.status.success) {
          return response;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  updateSolicitudPersonaJuridica(
    solicitudPersonId: number,
    ubigeoId: number,
    direccionCompleta: string,
    referencia: string
  ): Observable<any> {
    const url = `${Const.API_ENTIDAD}v1/solicitudentidad/persona/${solicitudPersonId}`;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        ubigeoId: ubigeoId || null,
        direccionCompleta: direccionCompleta || null,
        referenciaDireccion: referencia || null,
      },
    };
    return this.http.put(url, tramaEnvio);
  }

  registerOrUpdateAdminRequest(
    body: any,
    document: File,
    documentBase64: string,
    flag: number,
    fileChanged: boolean,
    entity?: EntityRequest
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        solicitud: {
          solicitudEntidadId:
            flag === 0 ? null : entity.solicitudEntidad.solicitudEntidadId,
          sector: body.governmentSector,
          nivelGobierno: body.governmentLevel,
          tipo: 1,
          tipoEntidad: 0,
          aceptoTerminosCondiciones: 1,
          numeroIntentos: 0,
          aplicacionId: Const.APPLICATION_ID,
          estadoSolicitud: 40,
        },
        listaSolicitudAdjunto:
          fileChanged === false
            ? []
            : [
                {
                  solicitudArchivoId:
                    flag === 0
                      ? null
                      : entity.archivoSolicitud.solicitudArchivoId,
                  nombreArchivo: `${getMiliseconds()}.${
                    document.name.split('.')[
                      document.name.split('.').length - 1
                    ]
                  }`,
                  nombreRealArchivo: `${document.name}`,
                  tipoArchivo: 1,
                  archivo: documentBase64.split('base64,')[1],
                },
              ],
        listaSolicitudPersona: [
          // ---------------------------------- Persona Juridica ---------------------------------- //
          {
            solicitudPersonaId:
              flag === 0 ? null : entity.personaJuridica.solicitudPersonaId,
            rolEntidadId: 0,
            tipoPersona: 23,
            tipoDocumento: 6,
            numeroDocumento: body.ruc,
            validar: body.rucValidated,
            razonSocial: body.businessName.toUpperCase(),
            nombreComercial: '',
            sigla: '',
            nombres: '',
            apellidoPaterno: '',
            apellidoMaterno: '',
            apellidoCasada: '',
            fechaNacimiento: '',
            sexo: null,
            estadoCivil: 0,
            imagen: '',
            cargoId: '',
            puestoTrabajoId: 0,
            fechaInscripcion: '',
            fechaInicioActividades: '',
            actividadEconomicaPrincipal: '',
            condicionContribuyente: '',
            estadoConstribuyente: '',
            paisId: '',
            ubigeoId: body.ubigeoId,
            direccionCompleta: body.direction,
            referenciaDireccion: '',
            correoPrincipal: '',
            correoSecundario: '',
            correoLaboral: '',
            telefonoFijo: '',
            celularPrincipal: '',
            celularSecundario: '',
            celularLaboral: '',
            rutaPaginaWeb: '',
            lugarNacimiento: '',
          },
          // ---------------------------------- Persona Natural ---------------------------------- //
          {
            solicitudPersonaId:
              flag === 0 ? null : entity.personaNatural.solicitudPersonaId,
            rolEntidadId: 100,
            tipoPersona: 22,
            tipoDocumento: body.typeDocument,
            numeroDocumento: body.numberDocument,
            validar: 0,
            razonSocial: '',
            nombreComercial: '',
            sigla: '',
            nombres: body.name.toUpperCase(),
            apellidoPaterno: body.fatherName.toUpperCase(),
            apellidoMaterno: body.motherName?.toUpperCase(),
            apellidoCasada: '',
            fechaNacimiento: moment(body.birthday, 'DD/MM/YYYY').toDate(),
            sexo: body.genre,
            estadoCivil: 0,
            imagen: '',
            cargoId: body.occupationObject.cargoId || '',
            puestoTrabajoId: 0,
            fechaInscripcion: '',
            fechaInicioActividades: '',
            actividadEconomicaPrincipal: '',
            condicionContribuyente: '',
            estadoConstribuyente: '',
            paisId: body.countryObject?.paisId || '',
            ubigeoId: 0,
            direccionCompleta: '',
            referenciaDireccion: '',
            correoPrincipal: body.email.toLowerCase(),
            correoSecundario: body.alternativeEmail.toLowerCase(),
            correoLaboral: body.email.toLowerCase(),
            telefonoFijo: '',
            celularPrincipal: body.phone,
            celularSecundario: body.alternatePhone,
            celularLaboral: body.phone,
            rutaPaginaWeb: '',
            lugarNacimiento: '',
          },
        ],
      },
    };
    if (flag === 0) {
      const url = `${Const.API_ENTIDAD}v1/solicitudentidad`;
      return this.http.post(url, tramaEnvio);
    } else {
      const url = `${Const.API_ENTIDAD}v1/solicitudentidad/${entity.solicitudEntidad.solicitudEntidadId}`;
      return this.http.put(url, tramaEnvio);
    }
  }
}
