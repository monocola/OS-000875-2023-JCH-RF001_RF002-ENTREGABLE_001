import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';
import { requestFilter } from 'src/app/utils/general';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Const } from './const';
import { ResponseRequest } from '../model/reponse-request';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import moment from 'moment';

@Injectable({
  providedIn: 'root',
})
export class PerfilesService extends PerfilesRepository {
  constructor(
    private http: HttpClient,
    private authenticationRepository: AuthenticationRepository
  ) {
    super();
  }

  getListaPerfilGrupo(): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfilgrupo/listar`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getIdentificacion(perfilId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          const code: string = response.payload.perfil.puestoCodigo;
          code.split('-').length > 2
            ? (response.payload.perfil.puestoCodigo = code?.split('-')[3] || '')
            : (response.payload.perfil.puestoCodigo = code);
          return response.payload.perfil;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getFuncionesData(perfilId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/funcion/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.perfilFuncion;
        } else {
          return null;
        }
      })
    );
  }

  getFormacionData(perfilId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/formacionAcademica/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          return null;
        }
      })
    );
  }

  getExperienceData(perfilId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfilExperiencias/${perfilId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload;
        } else {
          return null;
        }
      })
    );
  }

  getPerfilGrupo(id?: number): Observable<any[]> {
    const url = `${Const.API_CONVOCATORIA}v1/perfilgrupo${
      id ? '?id=' + id : ''
    }`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getConocimientosByType(tipoId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/capacitacion/conocimiento/${tipoId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getCarrerasBySituation(situationId: number): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/capacitacion/carrera/${situationId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getPerfiles(body?: any): Observable<any[]> {

    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const fechaIni = body?.fecha?.start || null;
    const fechaFin = body?.fecha?.end ? body?.fecha?.end : fechaIni;

    const fecIniFormatted = fechaIni
      ? moment(fechaIni).format('yyyy-MM-DD')
      : null;

    const fecFinFormatted = fechaFin
      ? moment(fechaFin).format('yyyy-MM-DD')
      : null;

    const params = requestFilter({
      codigoPuesto: body?.codigo,
      nombrePerfil: body?.nombrePerfil,
      regimenId: body?.regimen,
      organoId: body?.organo,
      unidadOrganicaId: body?.unidadOrganica,
      estado: body?.estado,
      fechaIni: fecIniFormatted,
      fechaFin: fecFinFormatted,
    });

    const url = `${Const.API_CONVOCATORIA}v1/perfil/filter/${entidadId}?${params}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.items;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  getRegimenesToCreate(): Observable<any[]> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const url = `${Const.API_CONVOCATORIA}v1/maestraDetalle/regimen/entidad/${entidadId}`;
    return this.http.get(url).pipe(
      map((response: ResponseRequest) => {
        if (response.payload.listaDetalles) {
          return response.payload.listaDetalles;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  setCodeToIdentificacion(val) {
    return;
  }

  saveOrUpdateIdentificacion(
    body: any,
    regimenId: any,
    perfilIdentificacionId = null
  ): Observable<any> {
    const entidadId = this.authenticationRepository.getCurrentUserValue
      .entidadId;
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        perfil: {
          perfilId: perfilIdentificacionId || null,
          entidadId,
          regimenLaboralId: regimenId,
          codigoRegimen: body.codigoRegimen,
          nombrePuesto: body.nombrePerfil || null,
          organoId: body.organo.organigramaId || null,
          unidadOrganicaId: body.unidadOrganica.organigramaId || null,
          nombreOrgano: body.organo.descripcion || null,
          siglaOrgano: body.organo.sigla || null,
          nombreUnidadOrganica: body.unidadOrganica.unidadOrganica || null,
          unidadFuncional: body.unidadFuncional || null,
          nivelOrganizacional: body.nivelOrganizacional || null,
          servidorCivilId: body.grupoServidoresCiviles?.id || null,
          familiaPuestoId: body.familiaPuestos?.id || null,
          rolId: body.rol?.id || null,
          nivelCategoriaId: body.nivel || null,
          puestoTipoId: body.puestoTipo || null,
          subNivelsubCategoria: body.subnivel || null,
          dependenciaFuncional: body.depFuncional || null,
          dependenciaJerarquica: body.depJerarquica || null,
          servidorCivilReporteId: body.grupoServidoresParaReportar || null,
          nroPosicionesCargo: body.numPosicionesACargo || null,
          puestoCodigo: body.familiaPuestos
            ? `${body.grupoServidoresCiviles.codigo}-${body.familiaPuestos.codigo}-${body.rol.codigo}-${body.codigoPuesto}`
            : body.codigoPuesto,
          misionPuesto: body.mision || null,
          nroPosicionesPuesto: body.numPosicionesPuesto || null,
          codigoPosicion: body.codigoPosicion || null,
          puestosCargo: body.nombrePuestosACargo || null,
          tipoPracticaId: body.tipoPractica || null,
          condicionPracticaId: body.condicion || null,
          indColegiatura: null,
          indHabilitacionProf: null,
          puestoEstructural: body.puestoEstructural || null,
        },
      },
    };

    let request = null;
    if (perfilIdentificacionId) {
      const url = `${Const.API_CONVOCATORIA}v1/perfil/${perfilIdentificacionId}`;
      request = this.http.put(url, tramaEnvio);
    } else {
      const url = `${Const.API_CONVOCATORIA}v1/perfil`;
      request = this.http.post(url, tramaEnvio);
    }

    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.payload.perfil) {
          const code: string = response.payload.perfil.puestoCodigo;
          code.split('-').length > 2
            ? (response.payload.perfil.puestoCodigo = code?.split('-')[3] || '')
            : (response.payload.perfil.puestoCodigo = code);
          return response.payload.perfil;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateFunciones(
    body,
    codigoRegimen,
    perfilId,
    perfilFuncionId = null
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        perfilFuncion: {
          perfilFuncionId,
          perfilId,
          codigoRegimen,
          condicionAtipica: body.condicionAtipica || null,
          sustentoCondicionAtipica: body.sustento || null,
          periocidadCondicionAtipicaId: body.condicionAtipicaRadio || null,
          coordinacionInterna: body.coordinacionInterna || null,
          coordinacionExterna: body.coordinacionExterna || null,
          lstFuncionDetalle:
            this.formatFunctions(body.funciones, body.funcionesToDelete) ||
            null,
          lstServidorCivil: JSON.stringify(body.servidoresCiviles || '')
            .slice(0, -1)
            .substring(1),
        },
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/perfil/funcion`;
    let request = perfilFuncionId
      ? this.http.put(url, tramaEnvio)
      : this.http.post(url, tramaEnvio);

    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return response.payload.perfilFuncion;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateFormacion(body, regimenId, perfilId): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil/formacionAcademica`;
    const tramaEnvio = {
      traceId: {
        traceId: 'string',
      },
      payload: {
        perfilId,
        regimenLaboralId: regimenId,
        indColegiatura: body.colegiatura,
        indHabilitacionProf: body.habProfesional,
        observaciones: body.observaciones,
        lstFormacionAcademica: this.setNivelesFormacion(body),
        lstConocimientos: this.setConocimientoTecnico(body)
          .concat(this.getArrayBasicos(body))
          .concat(
            this.getArray(body, 'cursosRequeridos', 2, 'TBL_PERFIL_CUR_TAL')
          )
          .concat(
            this.getArray(body, 'programasRequeridos', 3, 'TBL_PERFIL_DIP_ESP')
          )
          .concat(this.getArrayLvl(body, 'ofimatica', 4))
          .concat(this.getArrayLvl(body, 'idiomas', 5)),
      },
    };
    return this.http.post(url, tramaEnvio).pipe(
      map((response: ResponseRequest) => {
        if (response.payload.perfil) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  saveOrUpdateExperiencia(
    body: any,
    perfilId: any,
    perfilExperienciaId = null
  ): Observable<any> {
    const tramaEnvio = {
      trace: {
        traceId: 'string',
      },
      payload: {
        id: perfilExperienciaId,
        perfilId,
        anioExpTotal: body.yearExpGeneral || 0,
        mesExpTotal: body.monthsExpGeneral || 0,
        anioExReqPuesto: body.yearEspecificaA || 0,
        mesExReqPuesto: body.monthsEspecificaA || 0,
        anioExpSecPub: body.yearEspecificaB || 0,
        mesExpSecPub: body.monthsEspecificaB || 0,
        nivelMinPueId: body.nivelPuesto || null,
        aspectos: body.aspectosComplementarios || null,
        detalle: this.setDetalle(
          body.habilidades || body.experiencias,
          body.habilidadesToDelete || body.experienciasToDelete,
          1,
          perfilExperienciaId
        ),
        detalle2: this.setDetalle(
          body.requisitos,
          body.requisitosToDelete,
          2,
          perfilExperienciaId
        ),
      },
    };
    const url = `${Const.API_CONVOCATORIA}v1/perfilExperiencias`;
    let request = perfilExperienciaId
      ? this.http.put(url, tramaEnvio)
      : this.http.post(url, tramaEnvio);

    return request.pipe(
      map((response: ResponseRequest) => {
        if (response.payload) {
          return response.payload;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }

  setDetalle(array: any[], arrayToDelete: any[], tipoDato, perfilExpId?) {
    const modifyArray =
      array?.map((el, index) => {
        return {
          id: el.funcionDetalleId || null,
          tiDaExId: tipoDato,
          requisitoId: el.extra || null,
          descripcion: el.descripcion,
          estado: 1,
          orden: index,
          perfilExperienciaId: perfilExpId,
        };
      }) || [];
    const deleteArray =
      arrayToDelete?.map((el) => {
        return {
          id: el.funcionDetalleId || null,
          tiDaExId: tipoDato,
          requisitoId: el.extra || null,
          descripcion: el.descripcion,
          estado: 0,
          orden: null,
          perfilExperienciaId: perfilExpId,
        };
      }) || [];
    return modifyArray.concat(deleteArray);
  }

  formatFunctions(array: any[], arrayToDelete) {
    const modifyArray =
      array?.map((el, index) => {
        return {
          funcionDetalleId: el.funcionDetalleId || null,
          descripcion: el.descripcion,
          orden: index + 1,
          estado: 1,
        };
      }) || [];
    const deleteArray =
      arrayToDelete?.map((el, index) => {
        return {
          funcionDetalleId: el.funcionDetalleId || null,
          descripcion: el.descripcion,
          orden: index + 1,
          estado: 0,
        };
      }) || [];
    return modifyArray.concat(deleteArray);
  }

  formatServidores(array: any[]) {
    return JSON.stringify(array);
  }

  getArray(body, field, tipoConocimiento, cabeceraCod): any[] {
    const aux = [];
    if (body[field]?.length > 0) {
      body[field].map((el) => {
        aux.push({
          origen:
            typeof el.nombreCurso.value === 'object'
              ? el.nombreCurso?.value?.tipo
              : 0,
          cod_cabecera: cabeceraCod,
          perfilId: body.perfil,
          regimenLaboralId: body.regimen,
          perfilConocimientoId: el.id || null,
          tipoConocimientoId: tipoConocimiento,
          conocimientoId:
            typeof el.nombreCurso.value === 'object'
              ? el.nombreCurso?.value?.maeDetalleId ||
                el.nombreCurso?.value?.maeDetalleEntidadId
              : null,
          descripcionConocimiento:
            typeof el.nombreCurso.value === 'string'
              ? el.nombreCurso?.value
              : null,
          horas: el.horas?.value,
          nivelDominioId: null,
          estado: el.estado,
        });
      });
    }
    return aux;
  }

  getArrayLvl(body, field, tipoConocimiento): any[] {
    const aux = [];
    if (body[field]?.length > 0) {
      body[field].map((el) => {
        aux.push({
          perfilId: body.perfil,
          regimenLaboralId: body.regimen,
          perfilConocimientoId: el.id || null,
          tipoConocimientoId: tipoConocimiento,
          conocimientoId: null,
          descripcionConocimiento: el.nombre,
          horas: null,
          nivelDominioId: el.nivel,
          estado: el.estado,
        });
      });
    }
    return aux;
  }

  getArrayBasicos(body) {
    const aux = [];
    if (body.conocimientosBasicos?.length > 0) {
      body.conocimientosBasicos.map((el) => {
        aux.push({
          perfilId: body.perfil,
          regimenLaboralId: body.regimen,
          perfilConocimientoId: el.funcionDetalleId || null,
          tipoConocimientoId: 1,
          conocimientoId: null,
          descripcionConocimiento: el.descripcion,
          horas: null,
          nivelDominioId: null,
          estado: el.estado,
        });
      });
    }
    return aux;
  }

  setNivelesFormacion(body) {
    const aux = [];
    if (body.nivelesAcademicos.length > 0) {
      body.nivelesAcademicos.map((el) => {
        aux.push({
          formacionAcademicaId: el.formacionAcademicaId,
          nivelEducativoId: el.nivel,
          estadoNivelEducativoId: el.tipoNivel,
          situacionAcademicaId: el.grado,
          estadoSituacionAcademicaId: el.tipoGrado || null,
          nombreGrado: el.nombreGrado || null,
          lstCarreraFormaAcademica: this.setCarreras(
            el.carreras,
            el.carrerasToDelete
          ),
          estado: 1,
        });
      });
    }
    if (body.nivelesAcademicosToDelete?.length > 0) {
      body.nivelesAcademicosToDelete.map((el) => {
        aux.push({
          formacionAcademicaId: el.formacionAcademicaId,
          nivelEducativoId: el.nivel,
          estadoNivelEducativoId: el.tipoNivel,
          situacionAcademicaId: el.grado,
          estadoSituacionAcademicaId: el.tipoGrado || null,
          nombreGrado: el.nombreGrado || null,
          lstCarreraFormaAcademica: this.setCarreras(
            el.carreras,
            el.carrerasToDelete
          ),
          estado: 0,
        });
      });
    }
    return aux;
  }

  setCarreras(carreras: any[], carrerasToDelete: any[]) {
    const mantenerCarreras = carreras.map((el) => {
      return {
        carreraFormacionAcademicaId: el.carreraFormacionAcademicaId || null,
        carreraId: el.id,
        estado: 1,
      };
    });
    const deleteCarreras = carrerasToDelete.map((el) => {
      return {
        carreraFormacionAcademicaId: el.carreraFormacionAcademicaId || null,
        carreraId: el.id,
        estado: 0,
      };
    });
    return mantenerCarreras.concat(deleteCarreras);
  }

  setConocimientoTecnico(body): any[] {
    const aux = [];
    if (body.idConocimientosTecnicos || body.conocimientosTecnicos) {
      aux.push({
        perfilId: body.perfil,
        regimenLaboralId: body.regimen,
        perfilConocimientoId: body.idConocimientosTecnicos || null,
        tipoConocimientoId: 1,
        conocimientoId: null,
        descripcionConocimiento: body.conocimientosTecnicos,
        horas: null,
        nivelDominioId: null,
        estado: body.conocimientosTecnicos ? 1 : 0,
      });
    }
    return aux;
  }

  inactivatePerfil(perfilId: any): Observable<any> {
    const url = `${Const.API_CONVOCATORIA}v1/perfil?perfilId=${perfilId}&estado=0`;
    return this.http.put(url, {}).pipe(
      map((response: ResponseRequest) => {
        if (response.status.success) {
          return true;
        } else {
          throw new Error(response.status.error.messages[0]).message;
        }
      })
    );
  }
}
