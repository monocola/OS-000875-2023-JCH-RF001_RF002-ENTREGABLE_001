import { Injectable } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { forkJoin } from 'rxjs';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { Const } from 'src/app/@data/services/const';

@Injectable({
  providedIn: 'root',
})
export class HelperLey1401Service {
  unidadesOrganicas = [];
  organos = [];
  roles = [];

  tipoPracticas: DetalleMaestra[] = [];
  condicionPracticas: DetalleMaestra[] = [];

  estadosNiveles: DetalleMaestra[] = [];

  identificacionForm: FormGroup;
  funcionesForm: FormGroup;
  formacionAcademicaForm: FormGroup;
  experienciaForm: FormGroup;

  idIdentificacion = null;
  idFunciones = null;
  idFormacion = null;
  idExperiencia = null;

  idIdentificacionDuplicado = null;
  idFuncionesDuplicado = null;
  idFormacionDuplicado = null;
  idExperienciaDuplicado = null;

  regimenSelected = null;
  registroTerminado = false;

  createMode = true;
  duplicarMode = false;

  // Registros a desactivar
  funcionesToDelete = [];
  habilidadesToDelete = [];
  conocimientosBasicosToDelete = [];

  indexStepper = 0;

  conocimientoMap: Map<number, any[]> = new Map<number, any[]>();
  listaoriginal: any[] = [];
  formacionDeletedItems: any[] = [];

  constructor(
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private organoRepository: OrganoRepository,
    private perfilesService: PerfilesRepository,
    private maestraService: MaestraRepository,
    private toastService: ToastService,
    private fb: FormBuilder
  ) {}

  loadCombox() {
    const getTipoPracticas = this.getDetByCodigo('TBL_PER_CON_PRA');
    const getCondiciones = this.getDetByCodigo('TBL_PER_TIP_PRA');
    const getNivelesEstados = this.getDetByCodigo('TBL_PER_EST_NIV_EDU');
    const getOrganos = this.organoRepository.getOrganos(false);
    const getUnidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      false
    );
    forkJoin([
      getUnidadesOrganicas,
      getOrganos,
      getTipoPracticas,
      getCondiciones,
      getNivelesEstados,
    ]).subscribe(
      (results) => {
        this.unidadesOrganicas = results[0];
        this.organos = results[1];
        this.tipoPracticas = results[2].filter((i) => i.estadoRegistro !== '0');
        this.condicionPracticas = results[3].filter(
          (i) => i.estadoRegistro !== '0'
        );
        this.estadosNiveles = this.getActive(results[4]);
        if (this.idIdentificacion) {
          this.createMode = false;
          this.getProfileData();
        }
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );

    this.maestraService
      .getMaestraDetalleByCod('TBL_MAE_TIPO_CONO')
      .toPromise()
      .then((items) => {
        this.maestraService
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_CON_TEC)
              .maeDetalleId
          )
          .toPromise()
          .then((res: any) => {
            this.listaoriginal = res.listaoriginal;
          });
      });
  }

  getProfileData() {
    const stepOne = this.perfilesService.getIdentificacion(
      this.idIdentificacion
    );
    const stepTwo = this.perfilesService.getFuncionesData(
      this.idIdentificacion
    );
    const stepThree = this.perfilesService.getFormacionData(
      this.idIdentificacion
    );
    const stepFour = this.perfilesService.getExperienceData(
      this.idIdentificacion
    );
    forkJoin([stepOne, stepTwo, stepThree, stepFour]).subscribe((results) => {
      let count = 0;
      this.setIdentificacionData(results[0]);
      results[1] ? this.setFuncionesData(results[1]) : count++;
      results[2] && results[2].lstFormacionAcademica.length > 0
        ? this.setFormacionData(results[2])
        : count++;
      results[3] ? this.setExperienciaData(results[3]) : count++;
      if (count !== 0) {
        this.indexStepper = 4 - count;
      }
      if (this.duplicarMode) {
        this.indexStepper = 0;
      }
    });
  }

  setIdentificacionData(val) {
    this.idIdentificacion = val.perfilId;
    this.identificacionForm.patchValue({
      nombrePerfil:
        this.duplicarMode && !this.idIdentificacionDuplicado
          ? val.nombrePuesto + ' - DUPLICADO'
          : val.nombrePuesto,
      organo: this.organos.find((o) => o.organigramaId === val.organoId) || '',
      unidadOrganica:
        this.unidadesOrganicas.find(
          (uo) => uo.organigramaId === val.unidadOrganicaId
        ) || '',
      unidadFuncional: val.unidadFuncional,
      depJerarquica: val.dependenciaJerarquica,
      codigoPuesto: val.puestoCodigo,
      tipoPractica: val.tipoPracticaId,
      condicion: val.condicionPracticaId,
      mision: val.misionPuesto
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
      funciones: funciones,
    });

    this.funcionesForm.markAsPristine();

    if (initDuplicate) {
      this.idFunciones = null;
      this.idFuncionesDuplicado = val.perfilId;
    }
  }

  setFormacionData(val: any) {
    const initDuplicate = this.duplicarMode && !this.idFormacionDuplicado;
    this.idFormacion = true;
    this.conocimientosBasicosToDelete = [];

    this.formacionAcademicaForm.patchValue({
      nivelesAcademicos: this.setNivelesAcademicos(
        val.lstFormacionAcademica,
        initDuplicate
      ),
      conocimientosBasicos: this.setConocimientosBasicos(
        val.lstConocimientos,
        initDuplicate
      ),
      nivelesAcademicosToDelete: [],
    });
    this.formacionAcademicaForm.markAsPristine();
    if (initDuplicate) {
      this.idFormacion = false;
      this.idFormacionDuplicado = true;
    }
  }

  setExperienciaData(data1401) {
    const initDuplicate = this.duplicarMode && !this.idExperienciaDuplicado;
    this.habilidadesToDelete = [];
    this.idExperiencia = data1401.id;
    this.experienciaForm.patchValue({
      experiencias: this.setHabilidadesOrRequisitos(
        data1401.detalle,
        initDuplicate
      ),
    });

    if (initDuplicate) {
      this.idExperiencia = null;
      this.idExperienciaDuplicado = data1401.id;
    }

    this.experienciaForm.markAsPristine();
  }

  setConocimientosBasicos(array, initDuplicate) {
    return (
      array?.map((el) => {
        return {
          descripcion: el.descrConocimiento,
          perfilConocimientoId: el.perfilConocimientoId,
          estado: 1,
          extra: '',
          funcionDetalleId: !initDuplicate ? el.perfilConocimientoId : null,
        };
      }) || []
    );
  }

  setNivelesAcademicos(list: any[], initDuplicate) {
    return (
      list?.map((el, index) => {
        return {
          grado: el.situacionAcademicaId,
          nivel: el.nivelEducativoId,
          nombreGrado: el.nombreGrado || '',
          orden: index,
          tipoGrado: el.estadoSituacionAcademicaId,
          tipoNivel: el.estadoNivelEducativoId,
          carreras: this.setCarreras(
            el.lstCarreraFormaAcademica,
            initDuplicate
          ),
          carrerasToDelete: [],
          formacionAcademicaId: !initDuplicate ? el.formacionAcademicaId : null,
          estado: '1',
        };
      }) || []
    );
  }

  setCarreras(list: any[], initDuplicate) {
    return (
      list?.map((el) => {
        return {
          id: el.carreraId,
          carreraFormacionAcademicaId: !initDuplicate
            ? el.carreraFormacionAcademicaId || null
            : null,
          descripcion: el.descripcion,
          estado: '1',
        };
      }) || []
    );
  }

  setHabilidadesOrRequisitos(array, initDuplicate) {
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

  getActive(array: any[]) {
    return array.filter((el) => el.estadoRegistro === '1');
  }

  getDetByCodigo(codigo_cabecera) {
    return this.maestraService.getMaestraDetalleByCod(codigo_cabecera);
  }

  initializeForm() {
    this.identificacionForm = this.fb.group({
      nombrePerfil: ['', [Validators.required]],
      organo: ['', [Validators.required]],
      unidadOrganica: ['', [Validators.required]],
      unidadFuncional: [''],
      depJerarquica: ['', [Validators.required]],
      tipoPractica: ['', [Validators.required]],
      condicion: [{ value: '', disabled: true }, [Validators.required]],
      codigoPuesto: [''],
      mision: ['', [Validators.required]],
    });

    this.funcionesForm = this.fb.group({
      funciones: [[], Validators.required],
    });

    this.formacionAcademicaForm = this.fb.group({
      nivelesAcademicos: [[], Validators.required],
      nivelesAcademicosToDelete: [[]],
      conocimientosBasicos: [[]],
    });

    this.experienciaForm = this.fb.group({
      experiencias: [[]],
    });
  }

  getDetalle(type: string, maestras: any[]) {
    return (
      maestras.filter((m) => m.codigoCabecera === type)[0]
        .listMaestraDetalles || []
    );
  }

  initializeValues() {
    this.initializeForm();

    this.idIdentificacion = null;
    this.idFunciones = null;
    this.idFormacion = null;
    this.idExperiencia = null;

    this.regimenSelected = null;
    this.registroTerminado = false;
    this.funcionesToDelete = [];
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
