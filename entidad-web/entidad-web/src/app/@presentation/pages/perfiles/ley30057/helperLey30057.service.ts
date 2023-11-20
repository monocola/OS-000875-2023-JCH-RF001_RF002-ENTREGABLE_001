import { Injectable } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { LstMaestraConocimiento } from '../../../../@data/model/maestra/conocimiento';
import { Const } from '../../../../@data/services/const';
import { Autocompleateagrupado } from './formacion/autocompleateagrupado/autocompleateagrupado.component';

@Injectable({
  providedIn: 'root',
})
export class HelperLey30057Service {
  roles = [];
  organos = [];
  unidadesOrganicas = [];
  unidadesFuncionales = [];
  servidoresCiviles: any[] = [];
  servidoresCivilesAReportar: any[] = [];
  familiaPuestos: any[] = [];
  puestosTipo: DetalleMaestra[] = [];
  niveles = [];

  periodicidades: DetalleMaestra[] = [];

  estadosNiveles: DetalleMaestra[] = [];
  carreras: DetalleMaestra[] = [];

  cursos: any[] = [];
  programasEspecializacion: any[] = [];
  nivelesMinimosPuestos: DetalleMaestra[] = [];
  reqAdicionales: DetalleMaestra[] = [];

  identificacionForm: FormGroup;
  funcionesForm: FormGroup;
  formacionAcademicaForm: FormGroup;
  experienciaForm: FormGroup;

  regimenSelected = null;

  idIdentificacion = null;
  idFunciones = null;
  idFormacion = null;
  idExperiencia = null;

  idIdentificacionDuplicado = null;
  idFuncionesDuplicado = null;
  idFormacionDuplicado = null;
  idExperienciaDuplicado = null;

  registroTerminado = false;
  createMode = true;
  duplicarMode = false;

  // Registros a desactivar
  funcionesToDelete = [];
  habilidadesToDelete = [];
  requisitosToDelete = [];

  indexStepper = 0;
  globalList: LstMaestraConocimiento[] = [];
  listaConocimientoTecnico: Autocompleateagrupado;
  listaCursosEsp: Autocompleateagrupado;
  listaProgramEsp: Autocompleateagrupado;
  listaConOfi: Autocompleateagrupado;
  listaConIdio: Autocompleateagrupado;

  constructor(
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private organoRepository: OrganoRepository,
    private parameterRepository: ParameterRepository,
    private perfilesService: PerfilesRepository,
    private maestraService: MaestraRepository,
    private maestraEntService: MaestraEntidadRepository,
    private toastService: ToastService,
    private fb: FormBuilder
  ) { }

  loadCombox() {
    const getOrganos = this.organoRepository.getOrganos(true);
    const getServidores = this.perfilesService.getPerfilGrupo();
    const getNiveles = this.parameterRepository.getNivelesOrgano();
    const getPuestosTipo = this.getDetByCodigo('TBL_PER_PTO');
    const getPeriodicidades = this.getDetByCodigo('TBL_PER_PER_APL');
    const getNivelesEstados = this.getDetByCodigo('TBL_PER_EST_NIV_EDU');
    const getNivelesMin = this.getDetByCodigo('TBL_PER_NIV_MIN_PTO');
    const getReqAdicionales = this.getDetByCodigo('TBL_PER_TIP_REQ');
    const getUnidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      true
    );
    const getCursos = this.getDetByCodigo('TBL_PERFIL_CUR_TAL');
    const getProgramasEsp = this.getDetByCodigo('TBL_PERFIL_DIP_ESP');
    const getMaestraCursoDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_CUR_TAL'
    );
    const getProgramasEspDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_DIP_ESP'
    );

    forkJoin([
      getPuestosTipo,
      getUnidadesOrganicas,
      getOrganos,
      getNiveles,
      getServidores,
      getPeriodicidades,
      getNivelesEstados,
      getNivelesMin,
      getReqAdicionales,
      getCursos,
      getProgramasEsp,
      getMaestraCursoDetalleEnt,
      getProgramasEspDetalleEnt,
    ]).subscribe(
      (results) => {
        this.puestosTipo = results[0];
        this.unidadesOrganicas = results[1];
        this.organos = results[2];
        this.niveles = results[3];
        this.servidoresCiviles = results[4];
        this.servidoresCivilesAReportar = results[4];
        this.periodicidades = this.getActive(results[5]);
        this.estadosNiveles = this.getActive(results[6]);
        this.nivelesMinimosPuestos = this.getActive(results[7]);
        this.reqAdicionales = this.getActive(results[8]);
        this.cursos = this.setType(results[9], 1).concat(
          this.setType(results[11], 0)
        );
        this.programasEspecializacion = this.setType(results[10], 1).concat(
          this.setType(results[12], 0)
        );
        if (this.idIdentificacion) {
          this.createMode = false;
          this.getProfileData();
        }
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  getCoursesAndProgramas() {
    const getMaestraCurso = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_CUR_TAL'
    );
    const getProgramasEspecialidades = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_DIP_ESP'
    );
    const promise = new Promise((resolve, reject) => {
      forkJoin([getMaestraCurso, getProgramasEspecialidades])
        .toPromise()
        .then((results) => {
          this.cursos = this.cursos.concat(this.setType(results[0], 0));
          this.programasEspecializacion = this.programasEspecializacion.concat(
            this.setType(results[1], 0)
          );
          resolve(true);
        });
    });
    return promise;
  }

  getProfileData() {
    const getIdData = this.perfilesService.getIdentificacion(
      this.idIdentificacion
    );
    const getFuncData = this.perfilesService.getFuncionesData(
      this.idIdentificacion
    );
    const getFormacionData = this.perfilesService.getFormacionData(
      this.idIdentificacion
    );
    const getExpData = this.perfilesService.getExperienceData(
      this.idIdentificacion
    );
    forkJoin([getIdData, getFuncData, getFormacionData, getExpData]).subscribe(
      (stepsData) => {
        let count = 0;
        this.setIdentificacionData(stepsData[0]);
        stepsData[1] ? this.setFuncionesData(stepsData[1]) : count++;
        stepsData[2] && stepsData[2].lstFormacionAcademica.length > 0
          ? this.setFormacionData(stepsData[2])
          : count++;
        stepsData[3] ? this.setExperienciaData(stepsData[3]) : count++;

        if (count !== 0) {
          this.indexStepper = 4 - count;
        }

        if (this.duplicarMode) {
          this.indexStepper = 0;
        }
      }
    );
  }

  setIdentificacionData(val) {
    this.idIdentificacion = val.perfilId;
    this.identificacionForm.patchValue({
      nombrePerfil:
        this.duplicarMode && !this.idIdentificacionDuplicado
          ? val.nombrePuesto + ' - DUPLICADO'
          : val.nombrePuesto,
      mision: val.misionPuesto || '',
      organo: this.organos.find((o) => o.organigramaId === val.organoId) || '',
      unidadOrganica:
        this.unidadesOrganicas.find(
          (uo) => uo.organigramaId === val.unidadOrganicaId
        ) || '',
      unidadFuncional: val.unidadFuncional || '',
      nivelOrganizacional: val.nivelOrganizacional || '',
      grupoServidoresCiviles: this.servidoresCiviles.find(
        (s) => s.id === val.servidorCivilId
      ),
      nivel: val.nivelCategoriaId,
      puestoTipo: val.puestoTipoId,
      subnivel: val.subNivelsubCategoria,
      depJerarquica: val.dependenciaJerarquica,
      depFuncional: val.dependenciaFuncional,
      grupoServidoresParaReportar: val.servidorCivilReporteId,
      numPosicionesACargo: val.nroPosicionesCargo,
      codigoPuesto: this.duplicarMode
        ? val.unidadFuncional
          ? val.puestoCodigo
          : null
        : val.puestoCodigo,
      codigoPosicion: val.codigoPosicion,
      numPosicionesPuesto: val.nroPosicionesPuesto,
    });
    if (!(this.duplicarMode && !this.idIdentificacionDuplicado)) {
      this.identificacionForm.controls.codigoPuesto.disable();
    }
    this.getFamiliaPuestos(val.servidorCivilId, val.familiaPuestoId, val.rolId);
    this.identificacionForm.markAsPristine();
    if (this.duplicarMode && !this.idIdentificacionDuplicado) {
      this.idIdentificacion = null;
      this.idIdentificacionDuplicado = val.perfilId;
    }
  }

  setFuncionesData(funcion) {
    const initDuplicate = this.duplicarMode && !this.idFuncionesDuplicado;
    this.funcionesToDelete = [];
    this.idFunciones = funcion.perfilFuncionId;
    const funciones =
      funcion.lstFuncionDetalle?.map((fd) => {
        return {
          funcionDetalleId: !initDuplicate ? fd.funcionDetalleId : null,
          descripcion: fd.descripcion,
          extra: '',
          estado: 1,
          orden: fd.orden || '',
        };
      }) || [];

    funciones.sort((a, b) =>
      a.orden > b.orden ? 1 : b.orden > a.orden ? -1 : 0
    );

    const srvCiviles = `[${funcion.lstServidorCivil}]`;
    const servidoresCiviles = JSON.parse(srvCiviles);

    this.funcionesForm.patchValue({
      funciones,
      coordinacionExterna: funcion.coordinacionExterna,
      coordinacionInterna: funcion.coordinacionInterna,
      condicionAtipica: funcion.condicionAtipica,
      sustento: funcion.sustentoCondicionAtipica,
      condicionAtipicaRadio: funcion.periocidadCondicionAtipicaId,
      servidoresCiviles,
    });
    this.funcionesForm.markAsPristine();
    if (initDuplicate) {
      this.idFunciones = null;
      this.idFuncionesDuplicado = funcion.perfilId;
    }
  }

  setFormacionData(formacion) {
    this.idFormacion = true;
    this.maestraService.getMaestraDetalleByCod('TBL_MAE_TIPO_CONO')
      .subscribe(items => {
        forkJoin([
          this.maestraService.getMaestraConocimiento(items.find(item => item.codProg === Const.COD_CON_TEC).maeDetalleId),
          this.maestraService.getMaestraConocimiento(items.find(item => item.codProg === Const.COD_CUR_ESP).maeDetalleId),
          this.maestraService.getMaestraConocimiento(items.find(item => item.codProg === Const.COD_PRO_ESP).maeDetalleId),
          this.maestraService.getMaestraConocimiento(items.find(item => item.codProg === Const.COD_CON_OFI).maeDetalleId),
          this.maestraService.getMaestraConocimiento(items.find(item => item.codProg === Const.COD_CON_IDIO).maeDetalleId),
        ]).subscribe(
          (results) => {
            this.listaConocimientoTecnico = {
              listagrupado: results[0].conocimientoAgrupado,
              listoriginal: results[0].listaoriginal
            };
            this.listaCursosEsp = {
              listagrupado: results[1].conocimientoAgrupado,
              listoriginal: results[1].listaoriginal
            };
            this.listaProgramEsp = {
              listagrupado: results[2].conocimientoAgrupado,
              listoriginal: results[2].listaoriginal
            };
            this.listaConOfi = {
              listagrupado: results[3].conocimientoAgrupado,
              listoriginal: results[3].listaoriginal
            };
            this.listaConIdio = {
              listagrupado: results[4].conocimientoAgrupado,
              listoriginal: results[4].listaoriginal
            };
            this.globalList.push(...this.listaConocimientoTecnico.listoriginal);
            this.globalList.push(...this.listaCursosEsp.listoriginal);
            this.globalList.push(...this.listaProgramEsp.listoriginal);
            this.globalList.push(...this.listaConOfi.listoriginal);
            this.globalList.push(...this.listaConIdio.listoriginal);

            this.patchForm(formacion);
          },
          (err) => { }
        );
      });
  }

  patchForm(formacion) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    let conocimientos = formacion.lstConocimientos;
    conocimientos = conocimientos.map(item => {
      item.descripcion = item.descripcionConocimiento;
      item.descripcionCategoria = this.globalList.find(globalitem => globalitem.maeConocimientoId === item.maeConocimientoId).descripcionCategoria;
      return item;
    });
    this.formacionAcademicaForm.patchValue({
      nivelesAcademicos: this.setNivelesAcademicos(
        formacion.lstFormacionAcademica
      ),
      colegiatura: formacion.indColegiatura,
      habProfesional: formacion.indHabilitacionProf,
      conocimientosTecnicos:
        conocimientos.filter((c) => this.listaConocimientoTecnico.listoriginal.map(item => item.maeConocimientoId).includes(c.maeConocimientoId)),
      cursosRequeridos:
        conocimientos.filter((c) => this.listaCursosEsp.listoriginal.map(item => item.maeConocimientoId).includes(c.maeConocimientoId)),
      programasRequeridos:
        conocimientos.filter((c) => this.listaProgramEsp.listoriginal.map(item => item.maeConocimientoId).includes(c.maeConocimientoId)),
      ofimatica:
        conocimientos.filter((c) => this.listaConOfi.listoriginal.map(item => item.maeConocimientoId).includes(c.maeConocimientoId)),
      idiomas:
        conocimientos.filter((c) => this.listaConIdio.listoriginal.map(item => item.maeConocimientoId).includes(c.maeConocimientoId)),
      observaciones: formacion.observaciones || '',
      nivelesAcademicosToDelete: [],
      cursosRequeridosToDelete: [],
      programasRequeridosToDelete: [],
      ofimaticaToDelete: [],
      idiomasToDelete: [],
    });

    this.formacionAcademicaForm.markAsPristine();
    if (initDuplicate) {
      this.idFormacion = false;
      this.idFormacionDuplicado = true;
    }
  }

  setExperienciaData(exp) {
    this.habilidadesToDelete = [];
    this.requisitosToDelete = [];
    this.idExperiencia = exp.id;
    this.experienciaForm.patchValue({
      yearExpGeneral: exp.anioExpTotal,
      monthsExpGeneral: exp.mesExpTotal,
      yearEspecificaA: exp.anioExReqPuesto,
      monthsEspecificaA: exp.mesExReqPuesto,
      yearEspecificaB: exp.anioExpSecPub,
      monthsEspecificaB: exp.mesExpSecPub,
      nivelPuesto: exp.nivelMinPueId,
      aspectosComplementarios: exp.aspectos,
      habilidades: this.setHabilidadesOrRequisitos(exp.detalle),
      requisitos: this.setHabilidadesOrRequisitos(exp.detalle2),
    });

    if (this.duplicarMode && !this.idExperienciaDuplicado) {
      this.idExperiencia = null;
      this.idExperienciaDuplicado = exp.id;
    }

    this.experienciaForm.markAsPristine();
  }

  setCursoOrProgram(array: any[]) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    return (
      array?.map((el) => {
        return {
          horas: new FormControl(el.horas),
          id: !initDuplicate ? el.perfilConocimientoId || null : null,
          estado: '1',
          nombreCurso: this.getCurso(
            el.conocimientoId,
            el.origen,
            el.tipoConocimientoId
          ),
        };
      }) || []
    );
  }

  getCurso(cursoId, origen, tipoConocimientoId) {
    let curso = null;
    if (tipoConocimientoId === 2) {
      curso = this.cursos.find(
        (c) =>
          (c.maeDetalleId === cursoId || c.maeDetalleEntidadId === cursoId) &&
          c.tipo === Number(origen)
      );
    } else {
      curso = this.programasEspecializacion.find(
        (c) =>
          (c.maeDetalleId === cursoId || c.maeDetalleEntidadId === cursoId) &&
          c.tipo === Number(origen)
      );
    }
    return new FormControl({ value: curso, disabled: true });
  }

  setOfimaticaOrLang(array: any[]) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    return (
      array?.map((el) => {
        return {
          nombre: el.descripcionConocimiento,
          id: !initDuplicate ? el.perfilConocimientoId || null : null,
          estado: '1',
          nivel: el.nivelDominioId.toString(),
        };
      }) || []
    );
  }

  setNivelesAcademicos(list: any[]) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    return (
      list?.map((el, index) => {
        return {
          grado: el.situacionAcademicaId,
          nivel: el.nivelEducativoId,
          nombreGrado: el.nombreGrado || '',
          orden: index,
          tipoGrado: el.estadoSituacionAcademicaId,
          tipoNivel: el.estadoNivelEducativoId,
          carreras: this.setCarreras(el.lstCarreraFormaAcademica),
          carrerasToDelete: [],
          formacionAcademicaId: !initDuplicate ? el.formacionAcademicaId : null,
          estado: '1',
        };
      }) || []
    );
  }

  setCarreras(list: any[]) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    return list.map((el) => {
      return {
        id: el.carreraId,
        carreraFormacionAcademicaId: !initDuplicate
          ? el.carreraFormacionAcademicaId || null
          : null,
        descripcion: el.descripcion,
        estado: '1',
      };
    });
  }

  setHabilidadesOrRequisitos(habOrReg: any[]) {
    const initDuplicate = this.duplicarMode && !this.idExperienciaDuplicado;
    return (
      habOrReg?.map((el) => {
        return {
          descripcion: el.descripcion,
          estado: 1,
          extra: el.requisitoId || '',
          funcionDetalleId: !initDuplicate ? el.id : null,
        };
      }) || []
    );
  }

  getDetByCodigo(codigo_cabecera) {
    return this.maestraService.getMaestraDetalleByCod(codigo_cabecera);
  }

  setType(array: any[], type: number): any[] {
    if (array) {
      return array.map((el) => {
        return {
          ...el,
          tipo: type,
        };
      });
    } else return [];
  }

  getActive(array: any[]) {
    return array.filter((el) => el.estadoRegistro === '1');
  }

  getDetalle(type: string, maestras: any[]) {
    return (
      maestras.filter((m) => m.codigoCabecera === type)[0]
        .listMaestraDetalles || []
    );
  }

  getFamiliaPuestos(id: number, patchFamPuestoId?: any, patchRolId?: any) {
    if (id) {
      this.perfilesService.getPerfilGrupo(id).subscribe((res) => {
        console.log (res);
        this.familiaPuestos = res;
        if (patchFamPuestoId) {
          setTimeout(() => {
            this.identificacionForm.patchValue({
              familiaPuestos: this.familiaPuestos.find(
                (f) => f.id === patchFamPuestoId
              ),
            });
          }, 0);
          this.getRoles(patchFamPuestoId, patchRolId);
        }
      });
    }
  }

  getRoles(id: number, patchRolId?: number) {
    if (id)
      this.perfilesService.getPerfilGrupo(id).subscribe((res) => {
        this.roles = res;
        if (patchRolId) {
          setTimeout(() => {
            this.identificacionForm.patchValue({
              rol: this.roles.find((f) => f.id === patchRolId),
            });
            this.identificacionForm.markAsPristine();
          }, 0);
        }
      });
  }

  initializeValues() {
    this.initializeForms();
    this.regimenSelected = null;
    this.idIdentificacion = null;
    this.idFunciones = null;
    this.idFormacion = null;
    this.idExperiencia = null;
    this.funcionesToDelete = [];
    this.habilidadesToDelete = [];
    this.requisitosToDelete = [];
    this.registroTerminado = false;
    this.createMode = true;
    this.indexStepper = 0;
    this.duplicarMode = false;

    this.idIdentificacionDuplicado = null;
    this.idFuncionesDuplicado = null;
    this.idFormacionDuplicado = null;
    this.idExperienciaDuplicado = null;

    sessionStorage.removeItem('regimenSelected');
  }

  initializeForms() {
    this.identificacionForm = this.fb.group({
      nombrePerfil: ['', Validators.required],
      mision: ['', Validators.required],
      organo: ['', Validators.required],
      unidadOrganica: ['', Validators.required],
      unidadFuncional: '',
      nivelOrganizacional: ['', Validators.required],
      grupoServidoresCiviles: ['', Validators.required],
      familiaPuestos: [''],
      rol: ['', Validators.required],
      nivel: ['', Validators.required],
      puestoTipo: [''],
      subnivel: ['', Validators.required],
      depJerarquica: ['', Validators.required],
      depFuncional: [''],
      grupoServidoresParaReportar: ['', Validators.required],
      numPosicionesACargo: ['', Validators.required],
      codigoPuesto: ['', Validators.required],
      codigoPosicion: ['', Validators.required],
      numPosicionesPuesto: ['', Validators.required],
    });

    this.funcionesForm = this.fb.group({
      funciones: [[], Validators.required],
      condicionAtipica: ['', Validators.required],
      condicionAtipicaRadio: ['', Validators.required],
      sustento: ['', Validators.required],
      coordinacionInterna: ['', Validators.required],
      coordinacionExterna: ['', Validators.required],
      servidoresCiviles: ['', Validators.required],
    });

    this.formacionAcademicaForm = this.fb.group({
      nivelesAcademicos: [[], Validators.required],
      nivelesAcademicosToDelete: [[]],
      colegiatura: ['', Validators.required],
      habProfesional: ['', Validators.required],
      conocimientosTecnicos: [[]],
      idConocimientosTecnicos: [null],
      cursosRequeridos: [[]],
      cursosRequeridosToDelete: [[]],
      programasRequeridos: [[]],
      programasRequeridosToDelete: [[]],
      ofimatica: [[]],
      ofimaticaToDelete: [[]],
      idiomas: [[]],
      idiomasToDelete: [[]],
      observaciones: [''],
    });

    this.experienciaForm = this.fb.group({
      yearExpGeneral: [''],
      monthsExpGeneral: [''],
      yearEspecificaA: [''],
      monthsEspecificaA: [''],
      yearEspecificaB: [''],
      monthsEspecificaB: [''],
      nivelPuesto: [''],
      aspectosComplementarios: [''],
      habilidades: [[]],
      requisitos: [[]],
    });
  }
}
