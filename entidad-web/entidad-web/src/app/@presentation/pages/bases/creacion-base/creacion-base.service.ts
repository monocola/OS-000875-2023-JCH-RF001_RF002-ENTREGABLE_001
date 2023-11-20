import { Injectable } from '@angular/core';
import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { DetalleMaestra } from 'src/app/@data/model/detalleMaestra';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { OrganoRepository } from 'src/app/@domain/repository/organo.repository';
import { BehaviorSubject, forkJoin, of } from 'rxjs';
import { Organo } from 'src/app/@data/model/organo';
import { BasesPlantillasRepository } from 'src/app/@domain/repository/bases-plantillas.repository';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { Const } from 'src/app/@data/services/const';
import { PerfilesRepository } from 'src/app/@domain/repository/perfiles.repository';
import { SedesRepository } from 'src/app/@domain/repository/sede.repository';
import { mesesContratoValues } from 'src/app/utils/values';
import { catchError } from 'rxjs/operators';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { Utils } from '../../../../utils/utils';
import { MaestraEntidadRepository } from 'src/app/@domain/repository/maestra-entidad.repository';
import { Declaracionjurada } from '../../../../@data/model/bases/declaracionjurada';

@Injectable({
  providedIn: 'root',
})
export class CreacionBaseService {
  form1: FormGroup;
  form2: FormGroup;
  form3: FormGroup;
  form4: FormGroup;
  form5: FormGroup;
  form6: FormGroup;

  idBase: number = null;

  idStep1: number = null;
  idStep2: number = null;
  idStep3: number = null;
  idStep4: number = null;
  idStep5: number = null;
  idStep6: number = null;

  indexStepper = 0;

  createMode = true;
  duplicarMode = false;

  jerarquiaSelected = null;
  jerarquiaMode = 0; // 0: Todas las leyes, 1: Practicas

  // Variables de los 6 steps
  organos: Organo[] = [];
  unidadesOrganicas = [];
  organosEncargados: DetalleMaestra[];
  tipoPracticas: DetalleMaestra[];
  informeBases = [];
  informeBasesEsp = [];
  declaracionesJuradas = [];
  dependencias = [];
  dependenciasEncargadas = [];
  etapas: DetalleMaestra[] = [];
  criteriosDeEvaluacion = [];
  listaDeEvaluaciones = [];
  cambiosEvaluacion: boolean = false;
  peso: number = 0;
  punMin: number = 0;
  punMax: number = 0;
  _nroVacantes: string = '';
  frecuencias: DetalleMaestra[] = [];
  perfiles = [];
  condicionesTrabajo: DetalleMaestra[] = [];
  modalidadesContrato: DetalleMaestra[] = [];
  decJuradasServir: DetalleMaestra[] = [];
  sedes: any[] = [];
  mesesContrato = mesesContratoValues;
  tiposDeInforme = [];
  listaCronogramas = [];
  etapasRegistro: DetalleMaestra[] = [];

  declaJuradaServirOrEntidad: any[] = [];
  declaraJurada: Declaracionjurada[] = [];
  decJuradaToDelete: any[] = [];
  // Arreglo de observaciones por pasos
  observaciones: ObservacionStepBase[] = [
    { description: '', step: 0, resuelto: true },
    { description: '', step: 1, resuelto: true },
    { description: '', step: 2, resuelto: true },
    { description: '', step: 3, resuelto: true },
    { description: '', step: 4, resuelto: true },
    { description: '', step: 5, resuelto: true },
  ];
  editEmitter = new BehaviorSubject(null);
  baseSeleccionada = null;
  baseSeleccionadaEsp = null;
  estadoBase = null;
  disableAllFields = false;
  observacionesInit: ObservacionStepBase[] = [];

  constructor(
    private organoRepository: OrganoRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private maestraService: MaestraRepository,
    private fb: FormBuilder,
    private plantillasBaseService: BasesPlantillasRepository,
    private basesService: BasesRepository,
    private perfilesService: PerfilesRepository,
    private sedesService: SedesRepository,
    private maestraEntidadService: MaestraEntidadRepository
  ) {}

  // Cargar todos los combos de todos los pasos
  loadCombox() {
    const getOrganosEntidadRepository = this.organoRepository.getOrganos(true);
    const getUnidadesOrganicas = this.unidadOrganicaRepository.getUnidadesOrganicas(
      true
    );
    const getTipoPracticasDetalleMaestra = this.getDetByCodigo(
      'TBL_PER_CON_PRA'
    );
    const getPlantillas = this.plantillasBaseService.getPlantillasBase({});
    const getOrganosEncargados = this.getDetByCodigo('TIP_ORG_ENC');
    const getEtapas = this.basesService.getEtapas('TIP_ETA_PRO');
    const getFrecuenciaHorarios = this.getDetByCodigo('TIP_FREC_DIA');
    const getPerfiles = this.perfilesService.getPerfiles({
      estadoRevision: Const.EST_PERFILES_REVISADO,
    });
    const getCondicionesTrabajo = this.getDetByCodigo('TIP_CON_TRA');
    const getDeclaracionesServir = this.getDetByCodigo('TIP_DEC_JUR');
    const getSedes = this.sedesService.getSedesByFiltro(null);
    const getModalidadesContratacion = this.getDetByCodigo(
      'TBL_BASE_MODALIDAD_CONTRATO'
    );
    const getCriteriosDeEvaluacion = this.basesService.getCriteriosDeEvaluacion();
    const getTiposDeInforme = this.basesService.getTiposDeInforme('TIP_INF');
    const getEtapasRegistro = this.getDetByCodigo('TIP_ETA_RE');
    const getDeclaraJurada = this.getDetByCodigo('TIP_DEC_JUR');
    const getMaeDetalleEntidad = this.maestraEntidadService.getMaeDetalleEntByCod(
      'TIP_DEC_JUR'
    );
    forkJoin([
      getOrganosEntidadRepository,
      getTipoPracticasDetalleMaestra,
      getPlantillas,
      getOrganosEncargados,
      getEtapas,
      getFrecuenciaHorarios,
      getPerfiles,
      getCondicionesTrabajo,
      getSedes,
      getUnidadesOrganicas,
      getModalidadesContratacion,
      getCriteriosDeEvaluacion,
      getDeclaracionesServir,
      getTiposDeInforme,
      getEtapasRegistro,
      getDeclaraJurada,
      getMaeDetalleEntidad,
    ]).subscribe((results) => {
      this.organos = results[0];
      this.tipoPracticas = results[1];
      this.informeBases = this.setInformes(results[2], Const.INF_BASE_LEGAL);
      this.informeBasesEsp = this.setInformes(
        results[2],
        Const.INF_BASE_LEGAL_ESP
      );
      this.declaracionesJuradas = this.setInformes(
        results[2],
        Const.INF_DEC_JURADA
      );
      this.organosEncargados = results[3];
      this.etapas = results[4];
      this.frecuencias = results[5];
      this.perfiles = this.setPerfiles(results[6]);
      this.condicionesTrabajo = results[7];
      this.sedes = this.setSedes(results[8]);
      this.unidadesOrganicas = results[9];
      this.modalidadesContrato = results[10];
      this.criteriosDeEvaluacion = results[11];
      this.decJuradasServir = results[12];
      this.tiposDeInforme = results[13];
      this.etapasRegistro = results[14];
      this.declaJuradaServirOrEntidad = results[15];
      this.declaraJurada = this.setType(results[16], 0);
      if (this.idStep1) {
        this.createMode = false;
        this.getBaseInfo();
      }
    });
  }

  setInformes(informes: any[], codProgTipoInforme: string) {
    return informes
      .filter((base) => base.codProg === codProgTipoInforme)
      .map((ib) => {
        return {
          value: ib.informeDetalleId,
          description: ib.titulo,
        };
      });
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

  getDetByCodigo(codigo_cabecera) {
    return this.maestraService.getMaestraDetalleByCod(codigo_cabecera);
  }

  setSedes(sedesArray: any[]) {
    const lista = [];
    sedesArray.forEach((element) => {
      lista.push({
        ...element,
        valueToShow: ` ${element.nombreSede} - ${element.direccion}`,
      });
    });
    return lista;
  }

  // Traer los 6 GET, descomentar de a uno
  getBaseInfo() {
    const stepOne = this.basesService.getDataStep1(this.idBase);
    const stepTwo = this.basesService.getDataStep2(this.idBase);
    const stepThree = this.basesService.getDataStep3(this.idBase).pipe(
      catchError((error) => {
        return of(null);
      })
    );
    const stepFour = this.basesService.getDataStep4(this.idBase).pipe(
      catchError((error) => {
        return of(null);
      })
    );
    const stepSix = this.basesService.getDataStep6(this.idBase);
    forkJoin([stepOne, stepTwo, stepThree, stepFour, stepSix]).subscribe(
      (results) => {
        this.setStepOne(results[0]);

        if (results[1]?.length > 0) {
          this.setStepTwo(results[1]);
        }

        if (results[2]?.baseId) {
          this.setStepThree(results[2]);
        }

        if (results[3]) {
          this.setStepFour(results[3]);
        } else {
          if (this.listaCronogramas.length !== 0) {
            this.idStep5 = this.idBase;
          }
        }

        if (results[4]) {
          this.setStepSix(results[4]);
        }

        let count = 0;

        count++;
        if (results[1]?.length <= 0) {
          this.goStep (count);
          return;
        }

        count++;
        if (!results[2]?.baseId) {
          this.goStep (count);
          return;
        }

        count++;
        if (results[3]) {
          if (!this.validEvaluacion (results[3].baseEvaluacionDetalleDTOList)) {
            this.goStep (count);
            return;
          }
        } else {
          this.goStep (count);
          return;
        }

        count++;
        if (!results[4]) {
          this.goStep (count);
          return;
        }
        // results[1]?.length > 0 ? this.setStepTwo(results[1]) : count++;
        // results[2]?.baseId ? this.setStepThree(results[2]) : count++;
        // results[3] ? this.setStepFour(results[3]) : count++;
        // this.listaCronogramas.length === 0
        //   ? count++
        //   : (this.idStep5 = this.idBase);
        // results[4] ? this.setStepSix(results[4]) : count++;
        // setTimeout(() => {
        //   if (count !== 0) {
        //     this.indexStepper = 6 - count; // 6 es el numero de pasos
        //   }
        // }, 0);
      }
    );

    this.basesService.getListaDeEvaluaciones(this.idBase).subscribe((res) => {
      this.listaDeEvaluaciones = res;
      let p = 0,
        min = 0,
        max = 0;
      this.listaDeEvaluaciones.forEach((num) => {
        p += num.peso;
        min += (num.puntajeMinimo * num.peso) / 100;
        max += (num.puntajeMaximo * num.peso) / 100;
      });
      this.peso = p;
      this.punMin = min;
      this.punMax = max;
    });
  }

  validEvaluacion(baseEvaluacionDetalleDTOList: any []) {
    let returned: boolean = true;

    for (let index = 0; index < baseEvaluacionDetalleDTOList.length; index++) {
      if (!baseEvaluacionDetalleDTOList[index].baseEvaluacionId) {
        return false;
      }
    }

    return true;
  }

  goStep (index: number) {
    setTimeout(() => {
      if (index !== 0) {
        this.indexStepper = index; // 6 es el numero de pasos
      }
    }, 0);
  }

  // Setear los 6 GET
  setStepOne(resp) {
    console.log("resp cccccccc:",resp)
    let baseSelectACtivd = this.informeBasesEsp.find(
      (ibo) => ibo.value === resp.informeEspecificaId
    );
    this.idStep1 = resp.baseId;
    this.idBase = resp.baseId;
    this._nroVacantes = resp.nroVacantes || '';
    this.form1.patchValue({
      datoConcursoId: resp.datoConcursoId,
      nombreConcurso: resp.nombre,
      objetivo: resp.objetivo,
      organo:
        this.organos.find(
          (o) => o.organigramaId === resp.organoResponsableId
        ) || '',
      unidadOrganica:
        this.unidadesOrganicas.find(
          (uo) => uo.organigramaId === resp.unidadOrganicaId
        ) || '',
      organoEncargado: resp.organoEncargadoId || '',
      correo: resp.correo || '',
      telefono: resp.telefono || '',
      anexo: resp.anexo || '',
      numeroVacantes: resp.nroVacantes || '',
      tipoPractica: resp.tipoPracticaId || '',
      baseSeleccionada:
        [this.informeBases.find((ib) => ib.value === resp.informeDetalleId)] ||
        [],
      baseSeleccionadaEsp: baseSelectACtivd != null ? [baseSelectACtivd] : [],
    });
    if (this.jerarquiaMode === 0) {
      this.form1.get('tipoPractica').disable();
    } else {
      this.form1.get('unidadOrganica').disable();
      this.form1.get('organoEncargado').disable();
    }
    this.form1.updateValueAndValidity();
    this.form1.markAsPristine();
  }

  setStepTwo(val) {
    this.form2.patchValue({ vacantes: val });
  }

  setStepThree(val) {
    this.form3.getRawValue();
    if (val.declaracionJuradaDTOList.length === 0) {
      this.idStep3 = val.baseId;
      this.form3.patchValue({
        baseId: val.baseId,
        declaracionesJuradas: [],
        declaracionesJuradasServir: this.setDeclaracionesJuradasMaestra(
          this.decJuradasServir,
          val.baseId
        ),
        declaraJuradaRequeridosToDelete: this.setDeclaracionReqToDelete(
          this.decJuradaToDelete
        ),
      });
      this.patchDataDeclaracionJuradaRequeridos(
        this.setDeclaracionesJuradas(val.declaracionJuradaDTOList, false)
      );
    } else {
      this.idStep3 = val.baseId;
      let valuetoPatch = {
        baseId: val.baseId,
        declaracionesJuradas: [],
        declaracionesJuradasServir: this.setDeclaracionesJuradas(
          val.declaracionJuradaDTOList,
          true
        ),
        declaraJuradaRequeridosToDelete: this.setDeclaracionReqToDelete(
          this.decJuradaToDelete
        ),
      };
      (this.form3.controls.declaraJuradaRequeridos as FormArray).clear();

      this.form3.patchValue(valuetoPatch);
      this.patchDataDeclaracionJuradaRequeridos(
        this.setDeclaracionesJuradas(val.declaracionJuradaDTOList, false)
      );

      if (
        this.form3.value.declaracionesJuradasServir &&
        this.form3.value.declaracionesJuradasServir.length !== 0
      ) {
        this.decJuradasServir = this.form3.value.declaracionesJuradasServir;
      }
    }
  }

  setDeclaracionReqToDelete(data: any[]) {
    let dataDeclaraJuradaToDelete: any[] = [];
    data.forEach((element) => {
      dataDeclaraJuradaToDelete.push({
        declaracionId: element.declaracionId,
        tipoId: element.tipoId,
        orden: element.orden,
        descripcion: element.descripcion,
        estado: '0',
        isServir: '0',
      });
    });
    return dataDeclaraJuradaToDelete;
  }
  clearFormArray = (formArray: FormArray) => {
    while (formArray.length !== 0) {
      formArray.removeAt(0);
    }
  }
  patchDataDeclaracionJuradaRequeridos(datalis: any[]) {
    let dataParserDeclaraJurada = this.newParser(datalis);
    dataParserDeclaraJurada.forEach((data) => {
      (this.form3.controls.declaraJuradaRequeridos as FormArray).push(
        this.fb.group(
          {
            name: [{ value: data.name, disabled: true }, Validators.required],
            datasend: [data.datasend],
            filtros: [data.filtros],
            legacy: [true],
          },
          {
            validators: [Utils.notCorrectDataValidator],
          }
        )
      );
    });
  }
  newParser(data: any[]) {
    return data.map((item) => {
      return {
        name: item.descripcion,
        datasend: {
          declaracionId: item.declaracionId,
          maeDetalleEntidadId: item.tipoId,
          orden: item.orden,
          descripcion: item.descripcion,
          estadoRegistro: item.estado,
          isServir: item.isServir,
        },
        filtros: this.declaraJurada,
      };
    });
  }

  setDeclaracionesJuradasMaestra(declaraciones: any[], baseId: any) {
    let listaDeclaraServir: any[] = [];
    declaraciones.forEach((element) => {
      if (element.codProg === '1') {
        listaDeclaraServir.push({
          declaracionId: null,
          tipoId: element.maeDetalleId,
          orden: element.orden,
          descripcion: element.descripcion,
          estado: element.estadoRegistro,
          isServir: '1',
          idBase: baseId,
        });
      }
    });
    return listaDeclaraServir;
  }

  setDeclaracionesJuradas(declaraciones: any[], servir: boolean) {
    let isServirVal = null;
    if (servir) {
      isServirVal = '1';
    } else {
      isServirVal = '0';
    }
    return declaraciones.filter((decl1) => decl1.isServir === isServirVal);
  }

  setStepFour(res) {
    if (res) {
      this.form4.patchValue({
        observacion: res.observacion,
      });
      this.form4.markAsPristine();
      this.idStep4 = this.idBase;
    }
  }

  setStepSix(res) {
    if (res) {
      const informes = [];
      res.informes.forEach((element) => {
        informes.push({
          value: element.idInforme,
          description: element.nombreInforme,
          tipoInformeId: element.tipoInformeId,
        });
      });
      this.form6.patchValue({
        informes: informes,
        observacion: res.observaciones,
      });
      this.form6.markAsPristine();
      this.idStep6 = this.idBase;
    }
  }

  initializeValues() {
    this.idBase = null;
    this.idStep1 = null;
    this.idStep2 = null;
    this.idStep3 = null;
    this.idStep4 = null;
    this.idStep5 = null;
    this.idStep6 = null;
    this.createMode = true;
    this.duplicarMode = false;
    this.indexStepper = 0;
    this.jerarquiaSelected = null;
    this.listaCronogramas = [];
    this.jerarquiaMode = 0;
    this.estadoBase = null;
    this.baseSeleccionada = null;
    this.baseSeleccionadaEsp = null;

    this.disableAllFields = false;
    this.observaciones = [
      { description: '', step: 0, resuelto: true },
      { description: '', step: 1, resuelto: true },
      { description: '', step: 2, resuelto: true },
      { description: '', step: 3, resuelto: true },
      { description: '', step: 4, resuelto: true },
      { description: '', step: 5, resuelto: true },
    ];
    this.initializeForm();
    sessionStorage.removeItem('jerarquiaSelected');
  }

  initializeForm() {
    this.form1 = this.fb.group({
      datoConcursoId: [''],
      nombreConcurso: ['', Validators.required],
      objetivo: ['', Validators.required],
      organo: ['', Validators.required],
      unidadOrganica: ['', Validators.required],
      organoEncargado: ['', Validators.required],
      correo: [
        '',
        [
          Validators.required,
          Validators.pattern('^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$'),
        ],
      ],
      telefono: ['', Validators.required],
      anexo: ['', Validators.required],
      numeroVacantes: ['', Validators.required],
      tipoPractica: ['', Validators.required],
      baseSeleccionada: [[], Validators.required],
      baseSeleccionadaEsp: [[]],
    });

    if (this.jerarquiaMode === 0) {
      this.form1.get('tipoPractica').disable();
    } else {
      this.form1.get('unidadOrganica').disable();
      this.form1.get('organoEncargado').disable();
    }
    this.form1.updateValueAndValidity();

    this.form2 = this.fb.group({
      vacantes: [[], Validators.required],
    });

    this.form3 = this.fb.group({
      declaracionesJuradas: [[]],
      declaracionesJuradasServir: [[]],
      declaraJuradaRequeridos: new FormArray([]),
      declaraJuradaRequeridosToDelete: [[]],
    });

    this.form4 = this.fb.group({
      observacion: [''],
    });

    this.form5 = this.fb.group({
      etapa: ['', Validators.required],
      fechaInicio: [null, Validators.required],
      fechaFin: [null, Validators.required],
      actividad: ['', Validators.required],
      evaluacion: [''],
      responsable: ['', Validators.required],
    });

    this.form6 = this.fb.group({
      tipoDeInforme: [''],
      informes: [[], Validators.required],
      observacion: [''],
    });
  }

  setObservacion(description: string) {
    this.observaciones[this.indexStepper].description = description;
    this.observaciones[this.indexStepper].resuelto = false;
  }

  deleteObservacion() {
    this.observaciones[this.indexStepper].description = '';
    this.observaciones[this.indexStepper].resuelto = true;
  }

  editObservacion() {
    this.editEmitter.next(true);
  }

  resolverObservacion() {
    this.observaciones[this.indexStepper].resuelto = true;
  }

  getObservaciones(baseId) {
    return this.basesService.getMovimientosObs(baseId);
  }

  observacionesInicializar() {
    this.observacionesInit = [];
    this.observacionesInit = JSON.parse(JSON.stringify(this.observaciones));
  }

  observacionesNotChange() {
    return Utils.arraysEqual(this.observacionesInit, this.observaciones);
  }

  setPerfiles(perfiles: any) {
    switch (this.jerarquiaSelected.regimen.codProg) {
      case Const.MD_DL30057 :
        return perfiles.filter(item => item.regimenLaboralId === Number(Const.MD_DL30057));
        break;
      case Const.MD_DL276 :
        return perfiles.filter(item => item.regimenLaboralId === Number(Const.MD_DL276));
        break;
      case Const.MD_DL728 :
        return perfiles.filter(item => item.regimenLaboralId === Number(Const.MD_DL728));
        break;
      case Const.MD_DL1041 :
        return perfiles.filter(item => item.regimenLaboralId === Number(Const.MD_DL1041));
        break;
      case Const.MD_DL1057 :
        return perfiles.filter(item => item.regimenLaboralId === Number(Const.MD_DL1057));
        break;
      default :
        return perfiles;
        break;
    }
  }
}

export interface ObservacionStepBase {
  description: string;
  step: number;
  resuelto: boolean;
}
