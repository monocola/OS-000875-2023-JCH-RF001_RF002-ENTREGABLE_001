import { Injectable } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { Const } from 'src/app/@data/services/const';

@Injectable({
  providedIn: 'root',
})
export class HelperLey276Service {
  unidadesOrganicas = [];
  organos = [];
  roles = [];

  nivelesMinimosPuestos: DetalleMaestra[] = [];
  reqAdicionales: DetalleMaestra[] = [];

  cursos: any[] = [];
  programasEspecializacion: any[] = [];
  estadosNiveles: DetalleMaestra[] = [];

  identificacionForm: FormGroup;
  funcionesForm: FormGroup;
  formacionAcademicaForm: FormGroup;
  experienciaForm: FormGroup;

  regimenSelected = null;

  idIdentificacion = null;
  idFunciones = null;
  idFormacion = null;
  idExperiencia = null;

  duplicateMode = null;

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
  constructor(
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private organoRepository: OrganoRepository,
    private toastService: ToastService,
    private maestraService: MaestraService,
    private maestraEntService: MaestraEntidadRepository,
    private perfilesService: PerfilesRepository,
    private fb: FormBuilder
  ) {}

  loadCombox() {
    const getOrganos = this.organoRepository.getOrganos(false);
    const getUnidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      false
    );
    const getNivelesMin = this.getDetByCodigo('TBL_PER_NIV_MIN_PTO');
    const getReqAdicionales = this.getDetByCodigo('TBL_PER_TIP_REQ');
    const getCursos = this.getDetByCodigo('TBL_PERFIL_CUR_TAL');
    const getProgramasEsp = this.getDetByCodigo('TBL_PERFIL_DIP_ESP');
    const getNivelesEstados = this.getDetByCodigo('TBL_PER_EST_NIV_EDU');
    const getMaestraCursoDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_CUR_TAL'
    );
    const getProgramasEspDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_DIP_ESP'
    );

    forkJoin([
      getUnidadesOrganicas,
      getOrganos,
      getNivelesMin,
      getReqAdicionales,
      getCursos,
      getProgramasEsp,
      getMaestraCursoDetalleEnt,
      getProgramasEspDetalleEnt,
      getNivelesEstados,
    ]).subscribe(
      (results) => {
        this.unidadesOrganicas = results[0];
        this.organos = results[1];
        this.nivelesMinimosPuestos = this.getActive(results[2]);
        this.reqAdicionales = this.getActive(results[3]);
        this.cursos = this.setType(results[4], 1).concat(
          this.setType(results[6], 0)
        );
        this.programasEspecializacion = this.setType(results[5], 1).concat(
          this.setType(results[7], 0)
        );
        this.estadosNiveles = this.getActive(results[8]);

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

  getProfileData() {
    const getIdentificacionData = this.perfilesService.getIdentificacion(
      this.idIdentificacion
    );
    const getFuncionesData = this.perfilesService.getFuncionesData(
      this.idIdentificacion
    );
    const getFormacionData = this.perfilesService.getFormacionData(
      this.idIdentificacion
    );
    const getExperienciaData = this.perfilesService.getExperienceData(
      this.idIdentificacion
    );
    forkJoin([
      getIdentificacionData,
      getFuncionesData,
      getFormacionData,
      getExperienciaData,
    ]).subscribe((stepsLey276) => {
      let count = 0;
      this.setIdentificacionData(stepsLey276[0]);
      stepsLey276[1] ? this.setFuncionesData(stepsLey276[1]) : count++;
      stepsLey276[2] && stepsLey276[2].lstFormacionAcademica.length > 0
        ? this.setFormacionData(stepsLey276[2])
        : count++;
      stepsLey276[3] ? this.setExperienciaData(stepsLey276[3]) : count++;
      if (count !== 0) {
        this.indexStepper = 4 - count;
      }
      if (this.duplicarMode) {
        this.indexStepper = 0;
      }
    });
  }

  getCoursesAndProgramas() {
    const getMaestraCursoDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_CUR_TAL'
    );
    const getProgramasEspDetalleEnt = this.maestraEntService.getMaeDetalleEntByCod(
      'TBL_PERFIL_DIP_ESP'
    );
    const promise = new Promise((resolve) => {
      forkJoin([getMaestraCursoDetalleEnt, getProgramasEspDetalleEnt])
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

  setFormacionData(formacion276) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    const listaConocimientos = formacion276.lstConocimientos;
    this.idFormacion = true;
    this.formacionAcademicaForm.patchValue({
      nivelesAcademicos: this.setNivelesAcademicos(
        formacion276.lstFormacionAcademica
      ),
      colegiatura: formacion276.indColegiatura,
      habProfesional: formacion276.indHabilitacionProf,
      conocimientosTecnicos:
        listaConocimientos.filter(
          (c) => c.tipoConocimientoId === Const.COD_CONOCIM_TECNICO
        ) || [],
      idConocimientosTecnicos: !initDuplicate
        ? listaConocimientos.filter(
            (c) => c.tipoConocimientoId === Const.COD_CONOCIM_TECNICO
          )[0]?.perfilConocimientoId || null
        : null,
      cursosRequeridos: listaConocimientos.filter((c) => c.tipoConocimientoId === Const.COD_CURSOS_ESPECIAL) || [],
      programasRequeridos: listaConocimientos.filter((c) => c.tipoConocimientoId === Const.COD_PROGRAM_ESPECIAL) || [],
      ofimatica: listaConocimientos.filter((c) => c.tipoConocimientoId === Const.COD_CONOCIM_OFIMATICA) || [],
      idiomas: listaConocimientos.filter((c) => c.tipoConocimientoId === Const.COD_CONOCIM_IDIOMA) || [],
      observaciones: formacion276.observaciones || '',
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

  setNivelesAcademicos(listNivelesAcad: any[]) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    return (
      listNivelesAcad?.map((ln, index) => {
        return {
          grado: ln.situacionAcademicaId,
          nivel: ln.nivelEducativoId,
          nombreGrado: ln.nombreGrado || '',
          orden: index,
          tipoGrado: ln.estadoSituacionAcademicaId,
          tipoNivel: ln.estadoNivelEducativoId,
          carreras: this.setCarreras(ln.lstCarreraFormaAcademica),
          carrerasToDelete: [],
          formacionAcademicaId: !initDuplicate ? ln.formacionAcademicaId : null,
          estado: '1',
        };
      }) || []
    );
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

  getCurso(idCurso, origen, tipoConocimientoId) {
    let curso = null;
    if (tipoConocimientoId === 2) {
      curso = this.cursos.find(
        (c) =>
          (c.maeDetalleId === idCurso || c.maeDetalleEntidadId === idCurso) &&
          c.tipo === Number(origen)
      );
    } else {
      curso = this.programasEspecializacion.find(
        (c) =>
          (c.maeDetalleId === idCurso || c.maeDetalleEntidadId === idCurso) &&
          c.tipo === Number(origen)
      );
    }
    return new FormControl({ value: curso, disabled: true });
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
      puestoEstructural: val.puestoEstructural,
      depJerarquica: val.dependenciaJerarquica,
      depFuncional: val.dependenciaFuncional,
      numPosicionesACargo: val.nroPosicionesCargo,
      codigoPuesto: val.puestoCodigo,
    });
    this.identificacionForm.markAsPristine();
    if (this.duplicarMode && !this.idIdentificacionDuplicado) {
      this.idIdentificacion = null;
      this.idIdentificacionDuplicado = val.perfilId;
    }
  }

  setFuncionesData(val) {
    const initDuplicate = this.duplicarMode && !this.idFuncionesDuplicado;
    this.funcionesToDelete = [];
    this.idFunciones = val.perfilFuncionId;
    const funciones =
      val.lstFuncionDetalle?.map((fd) => {
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

    this.funcionesForm.patchValue({
      coordinacionInterna: val.coordinacionInterna,
      coordinacionExterna: val.coordinacionExterna,
      funciones: funciones,
    });
    this.funcionesForm.markAsPristine();
    if (initDuplicate) {
      this.idFunciones = null;
      this.idFuncionesDuplicado = val.perfilId;
    }
  }

  setExperienciaData(val) {
    this.habilidadesToDelete = [];
    this.requisitosToDelete = [];
    this.idExperiencia = val.id;
    this.experienciaForm.patchValue({
      yearExpGeneral: val.anioExpTotal,
      monthsExpGeneral: val.mesExpTotal,
      yearEspecificaA: val.anioExReqPuesto,
      monthsEspecificaA: val.mesExReqPuesto,
      yearEspecificaB: val.anioExpSecPub,
      monthsEspecificaB: val.mesExpSecPub,
      nivelPuesto: val.nivelMinPueId,
      aspectosComplementarios: val.aspectos,
      habilidades: this.setHabilidadesOrRequisitos(val.detalle),
      requisitos: this.setHabilidadesOrRequisitos(val.detalle2),
    });
    this.formacionAcademicaForm.markAsPristine();
    if (this.duplicarMode && !this.idExperienciaDuplicado) {
      this.idExperiencia = null;
      this.idExperienciaDuplicado = val.id;
    }
  }

  setHabilidadesOrRequisitos(array) {
    const initDuplicate = this.duplicarMode && !this.idExperienciaDuplicado;
    return (
      array?.map((el) => {
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

  setForm() {
    this.formacionAcademicaForm.patchValue({});
  }

  initializeForm() {
    this.identificacionForm = this.fb.group({
      nombrePerfil: ['', [Validators.required]],
      mision: ['', [Validators.required]],
      organo: ['', [Validators.required]],
      unidadOrganica: ['', [Validators.required]],
      puestoEstructural: ['', [Validators.required]],
      depJerarquica: ['', [Validators.required]],
      depFuncional: [''],
      numPosicionesACargo: ['', [Validators.required]],
      codigoPuesto: [''],
    });

    this.funcionesForm = this.fb.group({
      funciones: [[], Validators.required],
      coordinacionInterna: ['', Validators.required],
      coordinacionExterna: ['', Validators.required],
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

  getActive(array: any[]) {
    return array.filter((el) => el.estadoRegistro === '1');
  }

  getDetalle(type: string, maestras: any[]) {
    return (
      maestras.filter((m) => m.codigoCabecera === type)[0]
        .listMaestraDetalles || []
    );
  }

  initializeValues() {
    this.initializeForm();
    this.regimenSelected = null;
    this.idIdentificacion = null;
    this.idFunciones = null;
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
}
