import { Utils } from './../../../../utils/utils';
import { Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { ToastService } from './../../../@common-components/toast';
import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';
import { Component, OnInit, ViewChild, HostListener } from '@angular/core';
import { MatStepper } from '@angular/material/stepper';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { DialogGuardarConfiguracionPerfilComponent } from '../dialog-guardar-configuracion-perfil/dialog-guardar-configuracion-perfil.component';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-configuracion-requisitos-min',
  templateUrl: './configuracion-requisitos-min.component.html',
  styleUrls: ['./configuracion-requisitos-min.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: true },
    },
  ],
})
export class ConfiguracionRequisitosMinComponent implements OnInit {
  lstAniosExperiencia: any[] = [];
  lstAniosExperienciaExp: any[] = [];

  isVolverEnabled: boolean = false;
  isContinuarEnabled: boolean = false;
  isGuardarEnabled: boolean = false;

  isStepFormacion: boolean = false;
  isStepExperiencia: boolean = false;
  isStepOtrosReq: boolean = false;
  isStepDeclaracionJur: boolean = false;

  selectedPerfil: any;
  selectedPlantilla: number;
  selectedConfigPerfilId: number;

  isEnableAddExpGeneral: boolean = true;
  isEnableAddExpEspMateria: boolean = true;
  isEnableAddExpEspPublico: boolean = true;
  isEnableAddExpNivelEspecifico: boolean = true;

  idPerfil: number;
  idRegimen: number;
  idEntidad: number;
  idBase: number;

  bodySize = '100%';
  menuWidth = 275;

  lstResumen: any;
  lstPesos: any;
  pesoFAExpGeneral: number = 0;
  pesoELExpGeneral: number = 0;
  pesoELExpEspecifica: number = 0;
  pesoELExpJefatural: number = 0;
  pesoELInvestigacion: number = 0;

  puntosMinFormacionAcademica: number = 0;
  puntosMinExpLabGeneral: number = 0;
  puntosMinExpLabEspecifica: number = 0;

  puntosMaxFormacionAcademica: number = 0;
  puntosMaxExpLabGeneral: number = 0;
  puntosMaxExpLabEspecifica: number = 0;

  cantidadPesos: number = 5;

  resumen: any = {
    total: 0,
    puntosMin: 0,
    puntosMax: 0,
  };

  isConfigEstrategico: boolean;

  // Formación Académica
  lstCarreras: any[] = [];
  lstEspecialidades: any[] = [];
  lstEspecificaciones: any[] = [];
  lstEspecificacionPrograma: any[] = [];
  lstEspecificacionCurso: any[] = [];
  lstMaestraEspecificaciones: any[] = [];

  lstGradosFormacionACarrera: any[] = [];

  frmCarreraGradosAdicionales: FormGroup;
  frmEspecialidades: FormGroup;
  frmEspecificacionPrograma: FormGroup;
  frmEspecificacionCurso: FormGroup;

  // Experiencia Laboral
  lstExperienciaLaboral: any[] = [];

  lstExperienciaGeneral: any[] = [];
  lstExpEspecificaMateria: any[] = [];
  lstExpEspecificaPublico: any[] = [];
  lstExpJefatural: any[] = [];
  lstExpNivelEspecifico: any[] = [];

  listaConfigInvestigacion: any[] = [];
  listaConfigPublicacion: any[] = [];
  listaConfigEspecifInvestigacion: any[] = [];
  listaConfigEspecifPublicacion: any[] = [];

  lstAniosExperienciaGeneral: any[] = [];
  lstAniosExperienciaMateria: any[] = [];
  lstAniosExperienciaNivelEspec: any[] = [];

  frmExperienciaGeneral: FormGroup;
  frmExpLabEspecificaMateria: FormGroup;
  frmExpLabEspecificaPublico: FormGroup;
  frmExperienciaJefatural: FormGroup;
  frmExperienciaNivelEspecifico: FormGroup;
  frmExpLabInvestigacion: FormGroup;
  frmExpLabEspecifInvestigacion: FormGroup;
  frmExpLabEspecifPublicacion: FormGroup;

  // Otros Requisitos
  lstOtrosRequisitos: any;

  // Declaraciones Juradas
  lstDDJJGeneral: any[] = [];
  lstDDJJEspecifica: any[] = [];

  @ViewChild('stepper') private stepper: MatStepper;

  constructor(
    private configuracionReqMinService: ConfiguracionReqMinRepository,
    private fb: FormBuilder,
    private toast: ToastService,
    private router: Router,
    private matDialog: MatDialog
  ) {
    this.frmEspecificacionPrograma = this.fb.group({
      cantidadPrograma: new FormControl(null, Validators.required),
      puntajePrograma: new FormControl('', [
        Validators.required,
        Validators.pattern('^[0-9]*$'),
        Validators.minLength(8),
      ]),
      reqMin: new FormControl(null),
    });

    this.frmCarreraGradosAdicionales = this.fb.group({
      cantidadGrados: new FormControl(null, Validators.required),
      puntajeGrados: new FormControl(null, Validators.required),
    });

    this.frmEspecificacionCurso = this.fb.group({
      cantidadCurso: new FormControl(null, Validators.required),
      puntajeCurso: new FormControl('', [
        Validators.required,
        Validators.pattern('^[0-9]*$'),
        Validators.minLength(8),
      ]),
      reqMin: new FormControl(null),
    });

    this.frmEspecialidades = this.fb.group({});

    this.frmExperienciaGeneral = this.fb.group({
      expGenDesde: new FormControl(null, Validators.required),
      expGenHasta: new FormControl(null, Validators.required),
      expGenReqMin: new FormControl(null),
      expGenPuntaje: new FormControl('', Validators.required),

      desdeAnio: new FormControl(null, Validators.required),
      hastaAnio: new FormControl(null, Validators.required),
      puntajeAnio: new FormControl(null, Validators.required),
    });

    this.frmExpLabEspecificaMateria = this.fb.group({
      expEspMatDesde: new FormControl(null, Validators.required),
      expEspMatHasta: new FormControl(null, Validators.required),
      expEspMatReqMin: new FormControl(null),
      expEspMatPuntaje: new FormControl('', Validators.required),

      desdeAnio: new FormControl(null, Validators.required),
      hastaAnio: new FormControl(null, Validators.required),
      puntajeAnio: new FormControl(null, Validators.required),
    });

    this.frmExpLabEspecificaPublico = this.fb.group({
      expEspPubDesde: new FormControl(null, Validators.required),
      expEspPubHasta: new FormControl(null, Validators.required),
      expEspPubReqMin: new FormControl(null),
      expEspPubPuntaje: new FormControl('', Validators.required),

      desdeAnio: new FormControl(null, Validators.required),
      hastaAnio: new FormControl(null, Validators.required),
      puntajeAnio: new FormControl(null, Validators.required),
    });

    this.frmExperienciaJefatural = this.fb.group({
      expEspJefDesde: new FormControl(null, Validators.required),
      expEspJefHasta: new FormControl(null, Validators.required),
      expEspJefReqMin: new FormControl(null),
      expEspJefPuntaje: new FormControl('', Validators.required),
    });

    this.frmExperienciaNivelEspecifico = this.fb.group({
      expNivEspDesde: new FormControl(null, Validators.required),
      expNivEspHasta: new FormControl(null, Validators.required),
      expNivEspReqMin: new FormControl(null, Validators.required),
      expNivEspPuntaje: new FormControl(null, Validators.required),

      desdeAnio: new FormControl(null, Validators.required),
      hastaAnio: new FormControl(null, Validators.required),
      puntajeAnio: new FormControl(null, Validators.required),
    });

    this.frmExpLabInvestigacion = this.fb.group({
      investigacionReqMin: new FormControl(null, Validators.required),
    });

    this.frmExpLabEspecifInvestigacion = this.fb.group({
      expEspecInvestCantidad: new FormControl(null, Validators.required),
      expEspecInvestSuperior: new FormControl(null, Validators.required),
      expEspecInvestPuntaje: new FormControl(null, Validators.required),
    });

    this.frmExpLabEspecifPublicacion = this.fb.group({
      expEspecPublicacionCantidad: new FormControl(null, Validators.required),
      expEspecPublicacionSuperior: new FormControl(null, Validators.required),
      expEspecPublicacionPuntaje: new FormControl(null, Validators.required),
    });
  }

  ngOnInit(): void {
    this.selectedPerfil = JSON.parse(sessionStorage.getItem('selectedPerfil'));
    this.idPerfil = Number(this.selectedPerfil.perfilId);
    this.idRegimen = Number(this.selectedPerfil.regimenId);
    this.idEntidad = Number(this.selectedPerfil.entidadId);
    this.idBase = Number(this.selectedPerfil.baseId);

    this.selectedPlantilla = Number(localStorage.getItem('selectedPlantilla'));
    this.selectedConfigPerfilId = Number(
      localStorage.getItem('selectedconfigPerfilId')
    );
    this.validarConfiguracionPlantilla();
    this.cargarAnios();
    this.cargarAniosExp();
    this.cargarResumen();
    this.cargarPesos();
    this.cargarFormacionAcademica();
    this.cargarExperienciaLaboral();
    this.cargarOtrosRequisitos();
    this.cargarDeclaracionJurada();

    this.calcularPuntajeMinimoELGeneral();
    this.calcularPuntajeMinimoELEspecifico();
    this.onResized(this);
    this.validarBtnContinuar(0);
  }

  validarConfiguracionPlantilla() {
    switch (this.selectedPlantilla) {
      case 1:
        this.isConfigEstrategico = false;
        break;
      case 2:
        this.isConfigEstrategico = true;
        break;
      case 3:
        this.isConfigEstrategico = true;
        break;
      default:
        this.isConfigEstrategico = true;
        break;
    }
  }

  cargarAnios() {
    for (let i = 1; i <= 5; i++) {
      let anio = {
        valor: i,
        descripcion: this.fillLeft(i, 2),
      };
      this.lstAniosExperiencia[i - 1] = anio;
    }
  }

  cargarAniosExp() {
    for (let i = 1; i <= 30; i++) {
      let anios = {
        valor: i,
        descripcion: this.fillLeft(i, 2),
      };
      this.lstAniosExperienciaExp[i - 1] = anios;
    }
  }

  cargarPesos() {
    this.configuracionReqMinService
      .getPesosConfiguracion(this.idPerfil, this.idBase)
      .subscribe((mc) => {
        if (mc.items != null) {
          this.lstPesos = mc.items;
          console.log('Lista de pesos: ', this.lstPesos);
          mc.items.forEach((element) => {
            switch (element.tipoSeccion) {
              case 0:
                this.pesoFAExpGeneral = element.pesoSeccion;
                break;
              case 1:
                this.pesoELExpGeneral = element.pesoSeccion;
                break;
              case 2:
                this.pesoELExpEspecifica = element.pesoSeccion;
                break;
              case 3:
                this.pesoELInvestigacion = element.pesoSeccion;
                break;
              case 4:
                this.pesoELExpJefatural = element.pesoSeccion;
                break;
              // case 5:
              //   this.pesoELInvestigacion = element.pesoSeccion;
              //   break;
              default:
                break;
            }
          });
          this.calcularTotalPuntos();
          this.calcularResumenTotal();
        }
      });
  }

  cargarResumen() {
    this.configuracionReqMinService
      .getPuntajesResumen(this.idPerfil, this.idRegimen, this.idEntidad)
      .subscribe((mc) => {
        this.lstResumen = mc;
      });
  }

  @HostListener('window:resize', ['$event'])
  onResized(e) {
    if (innerWidth <= 1400) {
      this.menuWidth = 241;
    } else if (innerWidth > 1400) {
      this.menuWidth = 274;
    }
    this.bodySize = innerWidth - this.menuWidth + 'px';
  }

  /* I FORMACION ACADEMICA */

  cargarFormacionAcademica() {
    const getCarrearas = this.configuracionReqMinService.getCarreras(
      this.idPerfil,
      this.idBase
    );
    const getEspecialidades = this.configuracionReqMinService.getEspecialidades(
      this.idPerfil,
      this.idBase
    );
    const getEspecificaciones = this.configuracionReqMinService.getEspecificaciones(
      this.idPerfil,
      this.idBase
    );
    const getGradosCarreras = this.configuracionReqMinService.getGradosCarreras(
      this.idPerfil,
      this.idBase
    );

    forkJoin([
      getCarrearas,
      getEspecialidades,
      getEspecificaciones,
      getGradosCarreras,
    ]).subscribe((results) => {
      this.lstCarreras = results[0];
      this.lstEspecialidades = results[1];
      this.lstEspecificaciones = results[2].items;
      this.lstGradosFormacionACarrera = results[3];
      this.lstEspecificacionPrograma = this.lstEspecificaciones.filter(
        (item) => item.tipoEspecificacion === 162
      );
      this.lstEspecificacionCurso = this.lstEspecificaciones.filter(
        (item) => item.tipoEspecificacion === 161
      );
      console.log('Lista de Especialidades', this.lstEspecialidades);
      console.log(
        'Lista Especificaciones Programa',
        this.lstEspecificacionPrograma
      );
      console.log('Lista Especificaciones Curso', this.lstEspecificacionCurso);
      this.validarBtnContinuar(0);
      this.calcularPuntajeMinimoFA();
      this.calcularPuntajeMaximoFA();
    });
  }

  removeEspecificacionPrograma(index: number) {
    this.lstEspecificacionPrograma.splice(index, 1);
    this.validarBtnContinuar(0);
  }

  addEspecificacionPrograma() {
    let formValues = this.frmEspecificacionPrograma.getRawValue();
    let existeReqMin: boolean = false;
    if (formValues.reqMin) {
      this.lstEspecificacionPrograma.forEach((element) => {
        if (element.requisitoMinimo) {
          existeReqMin = true;
        }
      });
    }

    if (existeReqMin) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (
      formValues.cantidadPrograma == null ||
      !Number(formValues.cantidadPrograma)
    ) {
      this.toast.showToast(
        'Elija una cantidad de cursos válido',
        'info',
        'Atención'
      );
    } else if (
      !Number(formValues.puntajePrograma) &&
      formValues.puntajePrograma <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else if (
      !Number(formValues.puntajePrograma) &&
      formValues.cantidadPrograma <= 0
    ) {
      this.toast.showToast('Ingrese una cantidad válida', 'info', 'Atención');
    } else {
      const objPrograma = {
        confFormacEspecificaId: 0,
        perfilId: this.idPerfil,
        tipoEspecificacion: 0,
        cantidad: formValues.cantidadPrograma,
        puntaje: formValues.puntajePrograma,
        requisitoMinimo: formValues.reqMin,
        descripcion: 'Programas',
      };
      this.lstEspecificacionPrograma.push(objPrograma);
      this.frmEspecificacionPrograma.reset();
      this.calcularPuntajeMinimoFA();
      this.calcularPuntajeMaximoFA();
      this.validarBtnContinuar(0);
    }
  }

  removeGradosCarrera(iGrado: number) {
    this.lstEspecificacionCurso.splice(iGrado, 1);
  }

  addGradosCarreras() {
    let formValues = this.frmCarreraGradosAdicionales.getRawValue();
    let objGrados = {
      confPerfFormacCarreraDetalleId: null,
      perfilId: this.idPerfil,
      grado: formValues.cantidadGrados,
      puntaje: formValues.puntajeGrados,
      baseId: this.idBase,
    };
    this.lstGradosFormacionACarrera.push(objGrados);
    this.frmCarreraGradosAdicionales.reset();
  }

  removeEspecificacionCurso(index: number) {
    this.lstEspecificacionCurso.splice(index, 1);
    this.validarBtnContinuar(0);
  }

  addEspecificacionCurso() {
    let formValues = this.frmEspecificacionCurso.getRawValue();
    let existeReqMin: boolean = false;
    if (formValues.reqMin) {
      this.lstEspecificacionCurso.forEach((element) => {
        if (element.requisitoMinimo) {
          existeReqMin = true;
        }
      });
    }

    if (existeReqMin) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (
      formValues.cantidadCurso == null ||
      !Number(formValues.cantidadCurso)
    ) {
      this.toast.showToast(
        'Elija una cantidad de cursos válido',
        'info',
        'Atención'
      );
    } else if (
      !Number(formValues.puntajeCurso) ||
      formValues.puntajeCurso <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else if (
      !Number(formValues.puntajeCurso) ||
      formValues.cantidadCurso <= 0
    ) {
      this.toast.showToast('Ingrese una cantidad válida', 'info', 'Atención');
    } else {
      const objCurso = {
        confFormacEspecificaId: 0,
        perfilId: this.idPerfil,
        tipoEspecificacion: 1,
        cantidad: formValues.cantidadCurso,
        puntaje: formValues.puntajeCurso,
        requisitoMinimo: formValues.reqMin,
        descripcion: 'Cursos',
      };
      this.lstEspecificacionCurso.push(objCurso);
      this.frmEspecificacionCurso.reset();
      this.calcularPuntajeMinimoFA();
      this.calcularPuntajeMaximoFA();
      this.validarBtnContinuar(0);
    }
  }

  onCheckReqMinCarrera(e, index: number): void {
    this.lstCarreras.forEach((element) => {
      element.flagReqMin = null;
    });
    this.lstCarreras[index].flagReqMin = e;
    this.calcularPuntajeMinimoFA();
    this.calcularPuntajeMaximoFA();
    this.validarBtnContinuar(0);
  }

  onCheckReqMinEspecialidad(e, index: number): void {
    // this.lstEspecialidades.forEach((element) => {
    //   element.flagReqMin = null;
    // });
    this.lstEspecialidades[index].flagReqMin = e;
    this.validarBtnContinuar(0);
  }

  onCheckReqMinInvestigacion(e, index: number): void {
    this.listaConfigInvestigacion.forEach((element) => {
      element.requisito = null;
    });
    this.listaConfigInvestigacion[index].requisito = e;
  }

  onCheckReqMinPublicacion(e, index: number): void {
    this.listaConfigPublicacion.forEach((element) => {
      element.requisito = null;
    });
    this.listaConfigPublicacion[index].requisito = e;
  }

  onCheckReqMinCursos(e, index: number): void {
    this.lstEspecificacionCurso.forEach((element) => {
      element.requisitoMinimo = null;
    });
    this.lstEspecificacionCurso[index].requisitoMinimo = e;
    this.calcularPuntajeMinimoFA();
    this.calcularPuntajeMaximoFA();
    this.validarBtnContinuar(0);
  }

  onCheckReqMinProgramas(e, index: number): void {
    this.lstEspecificacionPrograma.forEach((element) => {
      element.requisitoMinimo = null;
    });
    this.lstEspecificacionPrograma[index].requisitoMinimo = e;
    this.calcularPuntajeMinimoFA();
    this.calcularPuntajeMaximoFA();
    this.validarBtnContinuar(0);
  }
  /* I EXPERIENCIA LABORAL */

  cargarExperienciaLaboral() {
    this.configuracionReqMinService
      .getExperiencialLaboral(this.idPerfil, this.idBase)
      .subscribe((mc) => {
        this.lstExperienciaLaboral = mc;
        console.log('Matriz experiencia laboral', this.lstExperienciaLaboral);
        this.lstExperienciaGeneral = this.lstExperienciaLaboral.filter(
          (item) => item.tipoExperiencia === 1
        );
        this.lstExpEspecificaMateria = this.lstExperienciaLaboral.filter(
          (item) => item.tipoExperiencia === 2
        );
        this.lstExpEspecificaPublico = this.lstExperienciaLaboral.filter(
          (item) => item.tipoExperiencia === 3
        );
        this.lstExpJefatural = this.lstExperienciaLaboral.filter(
          (item) => item.tipoExperiencia === 4
        );
        this.lstExpNivelEspecifico = this.lstExperienciaLaboral.filter(
          (item) => item.tipoExperiencia === 5
        );

        if (
          this.lstExperienciaGeneral === null ||
          this.lstExperienciaGeneral.length <= 0
        ) {
          this.lstExperienciaGeneral = [
            {
              perfilId: this.idPerfil,
              tipoExperiencia: 1,
              descripcionNivel: null,
              nivelEducativoId: null,
              situacionAcademicaId: null,
              nivelEducativoDesc: null,
              sitAcademiDesc: null,
              baseId: this.idBase,
              listDetalleExperiencia: [],
              listModeloDetalleExperiencia: [],
            },
          ];
        } else if (
          this.isConfigEstrategico &&
          this.lstExperienciaGeneral[0].listDetalleExperiencia !== null &&
          this.lstExperienciaGeneral[0].listDetalleExperiencia.length > 0
        ) {
          this.isEnableAddExpGeneral = false;
        }

        if (
          this.lstExpEspecificaMateria === null ||
          this.lstExpEspecificaMateria.length <= 0
        ) {
          this.lstExpEspecificaMateria = [
            {
              perfilId: this.idPerfil,
              tipoExperiencia: 2,
              descripcionNivel: null,
              nivelEducativoId: null,
              situacionAcademicaId: null,
              nivelEducativoDesc: null,
              sitAcademiDesc: null,
              baseId: this.idBase,
              listDetalleExperiencia: [],
              listModeloDetalleExperiencia: [],
            },
          ];
        } else if (
          this.isConfigEstrategico &&
          this.lstExpEspecificaMateria[0].listDetalleExperiencia !== null &&
          this.lstExpEspecificaMateria[0].listDetalleExperiencia.length > 0
        ) {
          this.isEnableAddExpEspMateria = false;
        }

        if (
          this.lstExpEspecificaPublico === null ||
          this.lstExpEspecificaPublico.length <= 0
        ) {
          this.lstExpEspecificaPublico = [
            {
              perfilId: this.idPerfil,
              tipoExperiencia: 3,
              descripcionNivel: null,
              nivelEducativoId: null,
              situacionAcademicaId: null,
              nivelEducativoDesc: null,
              sitAcademiDesc: null,
              baseId: this.idBase,
              listDetalleExperiencia: [],
              listModeloDetalleExperiencia: [],
            },
          ];
        } else if (
          this.isConfigEstrategico &&
          this.lstExpEspecificaPublico[0].listDetalleExperiencia !== null &&
          this.lstExpEspecificaPublico[0].listDetalleExperiencia.length > 0
        ) {
          this.isEnableAddExpEspPublico = false;
        }

        if (this.lstExpJefatural.length <= 0 && this.isConfigEstrategico) {
          this.lstExpJefatural = [
            {
              perfilId: this.idPerfil,
              tipoExperiencia: 4,
              descripcionNivel: null,
              nivelEducativoId: null,
              situacionAcademicaId: null,
              nivelEducativoDesc: null,
              sitAcademiDesc: null,
              baseId: this.idBase,
              listDetalleExperiencia: [],
              listModeloDetalleExperiencia: [],
            },
          ];
        }
        if (
          this.lstExpNivelEspecifico === null ||
          this.lstExpNivelEspecifico.length <= 0
        ) {
          this.lstExpNivelEspecifico = [
            {
              perfilId: this.idPerfil,
              tipoExperiencia: 5,
              descripcionNivel: null,
              nivelEducativoId: null,
              situacionAcademicaId: null,
              nivelEducativoDesc: null,
              sitAcademiDesc: null,
              baseId: this.idBase,
              listDetalleExperiencia: [],
              listModeloDetalleExperiencia: [],
            },
          ];
        } else if (
          this.isConfigEstrategico &&
          this.lstExpNivelEspecifico[0].listDetalleExperiencia !== null &&
          this.lstExpNivelEspecifico[0].listDetalleExperiencia.length > 0
        ) {
          this.isEnableAddExpNivelEspecifico = false;
        }

        console.log('lstExperienciaGeneral', this.lstExperienciaGeneral);
        console.log('lstExpEspecificaMateria', this.lstExpEspecificaMateria);
        console.log('lstExpEspecificaPublico', this.lstExpEspecificaPublico);
        console.log('lstExpJefatural', this.lstExpJefatural);
        console.log('lstExpNivelEspecifico', this.lstExpNivelEspecifico);

        this.calcularPuntajeMinimoELGeneral();
        this.calcularPuntajeMinimoELEspecifico();
        this.calcularPuntajeMaximoELGeneral();
        this.calcularPuntajeMaximoELEspecifica();
      });

    // Carga de lista de investigaciones
    this.configuracionReqMinService
      .getListaInvestigacionYPublicacion(this.idPerfil, this.idBase)
      .subscribe((mc) => {
        this.listaConfigInvestigacion = mc.lstInvestigacion;
        this.listaConfigPublicacion = mc.lstPublicacion;
        this.listaConfigEspecifInvestigacion = mc.lstEspecifInvestigacion;
        this.listaConfigEspecifPublicacion = mc.lstEspecifPublicacion;
      });
  }

  addExperienciaGeneral(iCab: number) {
    // En otra plantilla si registro una vez desaparece el boton agregar
    if (this.isConfigEstrategico) {
      // Si es otra plantilla
      if (
        this.lstExperienciaGeneral[iCab].listDetalleExperiencia != null &&
        this.lstExperienciaGeneral[iCab].listDetalleExperiencia.length <= 0
      ) {
        let formValues = this.frmExperienciaGeneral.getRawValue();
        let existeReqMin: boolean = false;

        if (formValues.expGenReqMin) {
          if (this.lstExperienciaGeneral[iCab].listDetalleExperiencia != null) {
            this.lstExperienciaGeneral[iCab].listDetalleExperiencia.forEach(
              (element) => {
                if (element.requisitoMinimo) {
                  existeReqMin = true;
                }
              }
            );
          }
        }

        if (existeReqMin) {
          this.toast.showToast(
            'Ya existe un requisito mínimo',
            'info',
            'Atención'
          );
        } else if (formValues.expGenDesde == null) {
          this.toast.showToast('Elija el año Desde', 'info', 'Atención');
        } else if (formValues.expGenHasta == null) {
          this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
        } else if (
          !Number(formValues.expGenPuntaje) ||
          formValues.expGenPuntaje <= 0
        ) {
          this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
        } else {
          const objDetalle = {
            desdeAnios: formValues.expGenDesde,
            hastaAnios: formValues.expGenHasta,
            requisitoMinimo: formValues.expGenReqMin,
            puntaje: formValues.expGenPuntaje,
          };
          if (this.lstExperienciaGeneral[iCab].listDetalleExperiencia == null) {
            this.lstExperienciaGeneral[iCab].listDetalleExperiencia = [];
          }
          this.lstExperienciaGeneral[iCab].listDetalleExperiencia.push(
            objDetalle
          );
          this.isEnableAddExpGeneral = false;
          this.validarBtnContinuar(0);
          this.frmExperienciaGeneral.reset();
        }
      }
    } else {
      // Si es plantilla de rol Directivo puede registrar múltiples experiencias

      let formValues = this.frmExperienciaGeneral.getRawValue();
      let existeReqMin: boolean = false;

      if (formValues.expGenReqMin) {
        if (this.lstExperienciaGeneral[iCab].listDetalleExperiencia != null) {
          this.lstExperienciaGeneral[iCab].listDetalleExperiencia.forEach(
            (element) => {
              if (element.requisitoMinimo) {
                existeReqMin = true;
              }
            }
          );
        }
      }

      if (existeReqMin) {
        this.toast.showToast(
          'Ya existe un requisito mínimo',
          'info',
          'Atención'
        );
      } else if (formValues.expGenDesde == null) {
        this.toast.showToast('Elija el año Desde', 'info', 'Atención');
      } else if (formValues.expGenHasta == null) {
        this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
      } else if (
        !Number(formValues.expGenPuntaje) ||
        formValues.expGenPuntaje <= 0
      ) {
        this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
      } else {
        const objDetalle = {
          desdeAnios: formValues.expGenDesde,
          hastaAnios: formValues.expGenHasta,
          requisitoMinimo: formValues.expGenReqMin,
          puntaje: formValues.expGenPuntaje,
        };
        if (this.lstExperienciaGeneral[iCab].listDetalleExperiencia == null) {
          this.lstExperienciaGeneral[iCab].listDetalleExperiencia = [];
        }
        this.lstExperienciaGeneral[iCab].listDetalleExperiencia.push(
          objDetalle
        );
        this.validarBtnContinuar(0);
        this.frmExperienciaGeneral.reset();
      }
    }
  }

  addExperienciaNivelEspecifico(iCab: number) {
    if (this.isConfigEstrategico) {
      // Si es otra plantilla
      if (this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.length <= 0) {
        let formValues = this.frmExperienciaNivelEspecifico.getRawValue();
        let existeReqMin: boolean = false;

        if (formValues.expNivEspReqMin) {
          if (this.lstExpNivelEspecifico[iCab].listDetalleExperiencia != null) {
            this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.forEach(
              (element) => {
                if (element.requisitoMinimo) {
                  existeReqMin = true;
                }
              }
            );
          }
        }

        if (existeReqMin) {
          this.toast.showToast(
            'Ya existe un requisito mínimo',
            'info',
            'Atención'
          );
        } else if (formValues.expNivEspDesde == null) {
          this.toast.showToast('Elija el año Desde', 'info', 'Atención');
        } else if (formValues.expNivEspHasta == null) {
          this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
        } else if (
          !Number(formValues.expNivEspPuntaje) ||
          formValues.expNivEspPuntaje <= 0
        ) {
          this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
        } else {
          const objDetalle = {
            desdeAnios: formValues.expNivEspDesde,
            hastaAnios: formValues.expNivEspHasta,
            requisitoMinimo: formValues.expNivEspReqMin,
            puntaje: formValues.expNivEspPuntaje,
          };
          if (this.lstExpNivelEspecifico[iCab].listDetalleExperiencia == null) {
            this.lstExpNivelEspecifico[iCab].listDetalleExperiencia = [];
          }
          this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.push(
            objDetalle
          );
          this.validarBtnContinuar(0);
          this.isEnableAddExpNivelEspecifico = false;
          this.frmExperienciaNivelEspecifico.reset();
        }
      }
    } else {
      let formValues = this.frmExperienciaNivelEspecifico.getRawValue();
      let existeReqMin: boolean = false;

      if (formValues.expNivEspReqMin) {
        if (this.lstExpNivelEspecifico[iCab].listDetalleExperiencia != null) {
          this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.forEach(
            (element) => {
              if (element.requisitoMinimo) {
                existeReqMin = true;
              }
            }
          );
        }
      }

      if (existeReqMin) {
        this.toast.showToast(
          'Ya existe un requisito mínimo',
          'info',
          'Atención'
        );
      } else if (formValues.expNivEspDesde == null) {
        this.toast.showToast('Elija el año Desde', 'info', 'Atención');
      } else if (formValues.expNivEspHasta == null) {
        this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
      } else if (
        !Number(formValues.expNivEspPuntaje) ||
        formValues.expNivEspPuntaje <= 0
      ) {
        this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
      } else {
        const objDetalle = {
          desdeAnios: formValues.expNivEspDesde,
          hastaAnios: formValues.expNivEspHasta,
          requisitoMinimo: formValues.expNivEspReqMin,
          puntaje: formValues.expNivEspPuntaje,
        };
        if (this.lstExpNivelEspecifico[iCab].listDetalleExperiencia == null) {
          this.lstExpNivelEspecifico[iCab].listDetalleExperiencia = [];
        }
        this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.push(
          objDetalle
        );
        this.validarBtnContinuar(0);
        this.frmExperienciaNivelEspecifico.reset();
      }
    }
  }

  addExperienciaEspecInvestigacion(iCab: number) {
    let formValues = this.frmExpLabEspecifInvestigacion.getRawValue();
    let existeSuperior: boolean = false;

    if (formValues.expEspecInvestSuperior) {
      if (this.listaConfigEspecifInvestigacion[iCab] != null) {
        this.listaConfigEspecifInvestigacion.forEach((element) => {
          if (element.superior) {
            existeSuperior = true;
          }
        });
      }
    }

    if (existeSuperior) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (formValues.expEspecInvestCantidad == null) {
      this.toast.showToast(
        'Elija una cantidad de investigaciones',
        'info',
        'Atención'
      );
    } else if (
      !Number(formValues.expEspecInvestPuntaje) ||
      formValues.expEspecInvestPuntaje <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objConfig = {
        configId: null,
        tipoId: null,
        tipoDescripcion: 'Investigación',
        cantidad: formValues.expEspecInvestCantidad,
        superior: formValues.expEspecInvestSuperior,
        puntaje: formValues.expEspecInvestPuntaje,
        baseId: this.idBase,
        perfilId: this.idPerfil,
      };
      if (this.listaConfigEspecifInvestigacion[iCab] == null) {
        this.listaConfigEspecifInvestigacion[iCab] = [];
      }
      this.listaConfigEspecifInvestigacion.push(objConfig);
      this.frmExpLabEspecifInvestigacion.reset();
    }
  }

  addExperienciaEspecPublicacion(iCab: number): void {
    let formValues = this.frmExpLabEspecifPublicacion.getRawValue();
    let existeSuperior: boolean = false;

    if (formValues.expEspecPublicacionSuperior) {
      if (this.listaConfigEspecifPublicacion[iCab] != null) {
        this.listaConfigEspecifPublicacion.forEach((element) => {
          if (element.superior) {
            existeSuperior = true;
          }
        });
      }
    }

    if (existeSuperior) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (formValues.expEspecPublicacionCantidad == null) {
      this.toast.showToast(
        'Elija una cantidad de investigaciones',
        'info',
        'Atención'
      );
    } else if (
      !Number(formValues.expEspecPublicacionPuntaje) ||
      formValues.expEspecPublicacionPuntaje <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objConfig = {
        configId: null,
        tipoId: null,
        tipoDescripcion: 'Publicación',
        cantidad: formValues.expEspecPublicacionCantidad,
        superior: formValues.expEspecPublicacionSuperior,
        puntaje: formValues.expEspecPublicacionPuntaje,
        baseId: this.idBase,
        perfilId: this.idPerfil,
      };
      if (this.listaConfigEspecifPublicacion[iCab] == null) {
        this.listaConfigEspecifPublicacion[iCab] = [];
      }
      this.listaConfigEspecifPublicacion.push(objConfig);
      this.frmExpLabEspecifPublicacion.reset();
    }
  }

  addAniosExperienciaMateria(confPerfExpeLaboralId: number, iCab: number) {
    let formValues = this.frmExpLabEspecificaMateria.getRawValue();
    if (formValues.hastaAnio == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (!Number(formValues.puntajeAnio) || formValues.puntajeAnio <= 0) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objAnio = {
        confPerfExpeLaboralDetalleId: null,
        confPerfExpeLaboralId: confPerfExpeLaboralId,
        desdeAnios: formValues.desdeAnio,
        hastaAnios: formValues.hastaAnio,
        puntaje: formValues.puntajeAnio,
      };

      if (
        this.lstExpEspecificaMateria[iCab].listModeloDetalleExperiencia === null
      ) {
        this.lstExpEspecificaMateria[iCab].listModeloDetalleExperiencia = [];
      }
      this.lstExpEspecificaMateria[iCab].listModeloDetalleExperiencia.push(
        objAnio
      );
      this.frmExpLabEspecificaMateria.reset();
    }
  }

  addAniosExperienciaPublica(confPerfExpeLaboralId: number, iCab: number) {
    let formValues = this.frmExpLabEspecificaPublico.getRawValue();
    if (formValues.desdeAnio == null) {
      this.toast.showToast('Elija el año Desde', 'info', 'Atención');
    } else if (formValues.hastaAnio == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (!Number(formValues.puntajeAnio) || formValues.puntajeAnio <= 0) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objAnio = {
        confPerfExpeLaboralDetalleId: null,
        confPerfExpeLaboralId: confPerfExpeLaboralId,
        desdeAnios: formValues.desdeAnio,
        hastaAnios: formValues.hastaAnio,
        puntaje: formValues.puntajeAnio,
      };

      if (
        this.lstExpEspecificaPublico[iCab].listModeloDetalleExperiencia === null
      ) {
        this.lstExpEspecificaPublico[iCab].listModeloDetalleExperiencia = [];
      }
      this.lstExpEspecificaPublico[iCab].listModeloDetalleExperiencia.push(
        objAnio
      );
      this.frmExpLabEspecificaPublico.reset();
    }
  }

  removeAniosExperienciaGeneral(iCab: number, iAnio: number) {
    this.lstExperienciaGeneral[iCab].listModeloDetalleExperiencia.splice(
      iAnio,
      1
    );
  }

  removeAniosExperienciaMateria(iCab: number, iAnio: number) {
    this.lstExpEspecificaMateria[iCab].listModeloDetalleExperiencia.splice(
      iAnio,
      1
    );
  }

  removeAniosExperienciaNivelEspecifico(iCab: number, iAnio: number) {
    this.lstExpNivelEspecifico[iCab].listModeloDetalleExperiencia.splice(
      iAnio,
      1
    );
  }

  removeAniosExperienciaPublico(iCab: number, iAnio: number) {
    this.lstExpEspecificaPublico[iCab].listModeloDetalleExperiencia.splice(
      iAnio,
      1
    );
  }

  addAniosExperienciaGeneral(confPerfExpeLaboralId: number, iCab: number) {
    let formValues = this.frmExperienciaGeneral.getRawValue();
    if (formValues.desdeAnio == null) {
      this.toast.showToast('Elija el año Desde', 'info', 'Atención');
    } else if (formValues.hastaAnio == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (!Number(formValues.puntajeAnio) || formValues.puntajeAnio <= 0) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objAnio = {
        confPerfExpeLaboralDetalleId: null,
        confPerfExpeLaboralId: confPerfExpeLaboralId,
        desdeAnios: formValues.desdeAnio,
        hastaAnios: formValues.hastaAnio,
        puntaje: formValues.puntajeAnio,
      };

      if (
        this.lstExperienciaGeneral[iCab].listModeloDetalleExperiencia === null
      ) {
        this.lstExperienciaGeneral[iCab].listModeloDetalleExperiencia = [];
      }
      this.lstExperienciaGeneral[iCab].listModeloDetalleExperiencia.push(
        objAnio
      );
      this.frmExperienciaGeneral.reset();
    }
  }

  addAniosExperienciaNivelEspecifico(
    confPerfExpeLaboralId: number,
    iCab: number
  ) {
    let formValues = this.frmExperienciaNivelEspecifico.getRawValue();
    if (formValues.desdeAnio == null) {
      this.toast.showToast('Elija el año Desde', 'info', 'Atención');
    } else if (formValues.hastaAnio == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (!Number(formValues.puntajeAnio) || formValues.puntajeAnio <= 0) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objAnio = {
        confPerfExpeLaboralDetalleId: null,
        confPerfExpeLaboralId: confPerfExpeLaboralId,
        desdeAnios: formValues.desdeAnio,
        hastaAnios: formValues.hastaAnio,
        puntaje: formValues.puntajeAnio,
      };
      if (
        this.lstExpNivelEspecifico[iCab].listModeloDetalleExperiencia === null
      ) {
        this.lstExpNivelEspecifico[iCab].listModeloDetalleExperiencia = [];
      }
      this.lstExpNivelEspecifico[iCab].listModeloDetalleExperiencia.push(
        objAnio
      );
      this.frmExperienciaNivelEspecifico.reset();
    }
  }

  actualizarPuntajeCarrera(e, index: number) {
    this.lstCarreras[index].puntaje = e.target.value;
    this.calcularPuntajeMinimoFA();
    this.calcularPuntajeMaximoFA();
    this.validarBtnContinuar(0);
  }

  actualizarPuntajeExpLabEspecPublicacion(e, index: number) {
    this.listaConfigEspecifPublicacion[index].puntaje = e.target.value;
  }

  addExperienciaEspecMateria(iCab: number) {
    if (this.isConfigEstrategico) {
      if (
        this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.length <= 0
      ) {
        let formValues = this.frmExpLabEspecificaMateria.getRawValue();
        let existeReqMin: boolean = false;
        if (formValues.expEspMatReqMin) {
          if (
            this.lstExpEspecificaMateria[iCab].listDetalleExperiencia != null
          ) {
            this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.forEach(
              (element) => {
                if (element.requisitoMinimo) {
                  existeReqMin = true;
                }
              }
            );
          }
        }

        if (existeReqMin) {
          this.toast.showToast(
            'Ya existe un requisito mínimo',
            'info',
            'Atención'
          );
        } else if (formValues.expEspMatDesde == null) {
          this.toast.showToast('Elija el año Desde', 'info', 'Atención');
        } else if (formValues.expEspMatHasta == null) {
          this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
        } else if (
          !Number(formValues.expEspMatPuntaje) ||
          formValues.expEspMatPuntaje <= 0
        ) {
          this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
        } else {
          const objDetalle = {
            desdeAnios: formValues.expEspMatDesde,
            hastaAnios: formValues.expEspMatHasta,
            requisitoMinimo: formValues.expEspMatReqMin,
            puntaje: formValues.expEspMatPuntaje,
          };
          if (
            this.lstExpEspecificaMateria[iCab].listDetalleExperiencia == null
          ) {
            this.lstExpEspecificaMateria[iCab].listDetalleExperiencia = [];
          }
          this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.push(
            objDetalle
          );
          this.isEnableAddExpEspMateria = false;
          this.frmExpLabEspecificaMateria.reset();
        }
      }
    } else {
      let formValues = this.frmExpLabEspecificaMateria.getRawValue();
      let existeReqMin: boolean = false;
      if (formValues.expEspMatReqMin) {
        if (this.lstExpEspecificaMateria[iCab].listDetalleExperiencia != null) {
          this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.forEach(
            (element) => {
              if (element.requisitoMinimo) {
                existeReqMin = true;
              }
            }
          );
        }
      }

      if (existeReqMin) {
        this.toast.showToast(
          'Ya existe un requisito mínimo',
          'info',
          'Atención'
        );
      } else if (formValues.expEspMatDesde == null) {
        this.toast.showToast('Elija el año Desde', 'info', 'Atención');
      } else if (formValues.expEspMatHasta == null) {
        this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
      } else if (
        !Number(formValues.expEspMatPuntaje) ||
        formValues.expEspMatPuntaje <= 0
      ) {
        this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
      } else {
        const objDetalle = {
          desdeAnios: formValues.expEspMatDesde,
          hastaAnios: formValues.expEspMatHasta,
          requisitoMinimo: formValues.expEspMatReqMin,
          puntaje: formValues.expEspMatPuntaje,
        };
        if (this.lstExpEspecificaMateria[iCab].listDetalleExperiencia == null) {
          this.lstExpEspecificaMateria[iCab].listDetalleExperiencia = [];
        }
        this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.push(
          objDetalle
        );
        this.frmExpLabEspecificaMateria.reset();
      }
    }
  }

  addExperienciaEspecPublica(iCab: number) {
    let formValues = this.frmExpLabEspecificaPublico.getRawValue();

    let existeReqMin: boolean = false;
    if (formValues.expEspPubReqMin) {
      if (this.lstExpEspecificaPublico[iCab].listDetalleExperiencia != null) {
        this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.forEach(
          (element) => {
            if (element.requisitoMinimo) {
              existeReqMin = true;
            }
          }
        );
      }
    }

    if (existeReqMin) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (formValues.expEspPubDesde == null) {
      this.toast.showToast('Elija el año Desde', 'info', 'Atención');
    } else if (formValues.expEspPubHasta == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (
      !Number(formValues.expEspPubPuntaje) ||
      formValues.expEspPubPuntaje <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objDetalle = {
        desdeAnios: formValues.expEspPubDesde,
        hastaAnios: formValues.expEspPubHasta,
        requisitoMinimo: formValues.expEspPubReqMin,
        puntaje: formValues.expEspPubPuntaje,
      };
      if (this.lstExpEspecificaPublico[iCab].listDetalleExperiencia == null) {
        this.lstExpEspecificaPublico[iCab].listDetalleExperiencia = [];
      }
      // this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.push(
      //   objDetalle
      // );
      // this.frmExpLabEspecificaPublico.reset();

      if (this.isConfigEstrategico) {
        if (
          this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.length <= 0
        ) {
          this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.push(
            objDetalle
          );
          this.isEnableAddExpEspPublico = false;
          this.frmExpLabEspecificaPublico.reset();
        }
      } else {
        this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.push(
          objDetalle
        );
        this.frmExpLabEspecificaPublico.reset();
      }
    }
    this.validarBtnContinuar(1);
  }

  addExperienciaJefatural(iCab: number) {
    let formValues = this.frmExperienciaJefatural.getRawValue();
    let existeReqMin: boolean = false;
    if (formValues.expEspJefReqMin) {
      if (this.lstExpJefatural[iCab].listDetalleExperiencia != null) {
        this.lstExpJefatural[iCab].listDetalleExperiencia.forEach((element) => {
          if (element.requisitoMinimo) {
            existeReqMin = true;
          }
        });
      }
    }

    if (existeReqMin) {
      this.toast.showToast('Ya existe un requisito mínimo', 'info', 'Atención');
    } else if (formValues.expEspJefDesde == null) {
      this.toast.showToast('Elija el año Desde', 'info', 'Atención');
    } else if (formValues.expEspJefHasta == null) {
      this.toast.showToast('Elija el año Hasta', 'info', 'Atención');
    } else if (
      !Number(formValues.expEspJefPuntaje) ||
      formValues.expEspJefPuntaje <= 0
    ) {
      this.toast.showToast('Ingrese un puntaje válido', 'info', 'Atención');
    } else {
      const objDetalle = {
        desdeAnios: formValues.expEspJefDesde,
        hastaAnios: formValues.expEspJefHasta,
        requisitoMinimo: formValues.expEspJefReqMin,
        puntaje: formValues.expEspJefPuntaje,
      };
      if (this.lstExpJefatural[iCab].listDetalleExperiencia == null) {
        this.lstExpJefatural[iCab].listDetalleExperiencia = [];
      }
      this.lstExpJefatural[iCab].listDetalleExperiencia.push(objDetalle);
      this.frmExperienciaJefatural.reset();
    }
  }

  removeExperienciaGeneral(iCab: number, iDet: number): void {
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia.splice(iDet, 1);
    this.validarBtnContinuar(1);
  }

  removeExperienciaEspecMateria(iCab: number, iDet: number): void {
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.splice(iDet, 1);
    this.validarBtnContinuar(1);
  }

  removeExperienciaEspecPublica(iCab: number, iDet: number): void {
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.splice(iDet, 1);
    this.validarBtnContinuar(1);
  }

  removeExperienciaEspecInvestigacion(iCab: number): void {
    this.listaConfigEspecifInvestigacion.splice(iCab, 1);
  }

  removeExperienciaEspecPublicacion(iCab: number): void {
    this.listaConfigEspecifPublicacion.splice(iCab, 1);
  }

  removeExperienciaJefatural(iCab: number, iDet: number): void {
    this.lstExpJefatural[iCab].listDetalleExperiencia.splice(iDet, 1);
  }

  onCheckReqMinExpLabGeneral(e, iCab: number, iDet: number): void {
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia.forEach(
      (element) => {
        element.requisitoMinimo = null;
      }
    );
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia[
      iDet
    ].requisitoMinimo = e;
    this.calcularPuntajeMinimoELGeneral();
    this.validarBtnContinuar(1);
  }

  onCheckReqMinExpLabMateria(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia.forEach(
      (element) => {
        element.requisitoMinimo = null;
      }
    );
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia[
      iDet
    ].requisitoMinimo = e;
    this.calcularPuntajeMinimoELEspecifico();
    this.validarBtnContinuar(1);
  }

  onCheckReqMinExpLabPublico(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia.forEach(
      (element) => {
        element.requisitoMinimo = null;
      }
    );
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia[
      iDet
    ].requisitoMinimo = e;
    this.calcularPuntajeMinimoELEspecifico();
    this.validarBtnContinuar(1);
  }

  onCheckReqMinExpLabJefatural(e, iCab: number, iDet: number): void {
    this.lstExpJefatural[iCab].listDetalleExperiencia.forEach((element) => {
      element.requisitoMinimo = null;
    });
    this.lstExpJefatural[iCab].listDetalleExperiencia[iDet].requisitoMinimo = e;
    this.calcularPuntajeMinimoELEspecifico();
  }

  onCheckReqMinExpLabNivEspec(e, iCab: number, iDet: number): void {
    this.lstExpNivelEspecifico[iCab].listDetalleExperiencia.forEach(
      (element) => {
        element.requisitoMinimo = null;
      }
    );
    this.lstExpNivelEspecifico[iCab].listDetalleExperiencia[
      iDet
    ].requisitoMinimo = e;
    this.calcularPuntajeMinimoELEspecifico();
    this.validarBtnContinuar(1);
  }

  onChangeExpLabExpGeneralDesde(e, iCab: number, iDet: number): void {
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia[
      iDet
    ].desdeAnios = e;
  }

  onChangeExpLabExpGeneralHasta(e, iCab: number, iDet: number): void {
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia[
      iDet
    ].hastaAnios = e;
  }

  onChangeExpLabExpMateriaDesde(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia[
      iDet
    ].desdeAnios = e;
  }

  onChangeExpLabExpMateriaHasta(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia[
      iDet
    ].hastaAnios = e;
  }

  onChangeExpLabPublicoDesde(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia[
      iDet
    ].desdeAnios = e;
  }

  onChangeExpLabPublicoHasta(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia[
      iDet
    ].hastaAnios = e;
  }

  onChangeExpLabJefaturalDesde(e, iCab: number, iDet: number): void {
    this.lstExpJefatural[iCab].listDetalleExperiencia[iDet].desdeAnios = e;
  }

  onChangeExpLabJefaturalHasta(e, iCab: number, iDet: number): void {
    this.lstExpJefatural[iCab].listDetalleExperiencia[iDet].hastaAnios = e;
  }

  actualizarPuntajeExpLabGeneral(e, iCab: number, iDet: number): void {
    this.lstExperienciaGeneral[iCab].listDetalleExperiencia[iDet].puntaje =
      e.target.value;
    this.calcularPuntajeMinimoELGeneral();
    this.calcularPuntajeMaximoELGeneral();
    this.validarBtnContinuar(1);
  }

  actualizarPuntajeExpLabMateria(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaMateria[iCab].listDetalleExperiencia[iDet].puntaje =
      e.target.value;
    this.calcularPuntajeMinimoELEspecifico();
    this.calcularPuntajeMaximoELEspecifica();
    this.validarBtnContinuar(1);
  }

  actualizarPuntajeExpLabPublico(e, iCab: number, iDet: number): void {
    this.lstExpEspecificaPublico[iCab].listDetalleExperiencia[iDet].puntaje =
      e.target.value;
    this.calcularPuntajeMinimoELEspecifico();
    this.calcularPuntajeMaximoELEspecifica();
    this.validarBtnContinuar(1);
  }

  actualizarPuntajeExpLabEspecInvestigacion(e, iCab: number): void {
    this.listaConfigEspecifInvestigacion[iCab].puntaje = e.target.value;
  }

  actualizarPuntajeExpLabJefatural(e, iCab: number, iDet: number): void {
    this.lstExpJefatural[iCab].listDetalleExperiencia[iDet].puntaje =
      e.target.value;
    this.calcularPuntajeMinimoELEspecifico();
    this.calcularPuntajeMaximoELEspecifica();
  }

  actualizarPuntajeExpLabNivEspec(e, iCab: number, iDet: number): void {
    this.lstExpNivelEspecifico[iCab].listDetalleExperiencia[iDet].puntaje =
      e.target.value;
    this.calcularPuntajeMinimoELEspecifico();
    this.calcularPuntajeMaximoELEspecifica();
    this.validarBtnContinuar(1);
  }

  /* I OTROS REQUISITOS */
  cargarOtrosRequisitos() {
    this.configuracionReqMinService
      .getRequisitosAdicionales(this.idPerfil, this.idBase)
      .subscribe((mc) => {
        this.lstOtrosRequisitos = mc.listaRequisitosAdicionales;
      });
  }

  onCheckReqMinOtrosRequisitos(e, index: number): void {
    this.lstOtrosRequisitos[index].requisitoMinimo = e;
    this.validarBtnContinuar(2);
  }

  /* I DECLARACION JURADA */

  cargarDeclaracionJurada() {
    console.log('Requisito aqui');
    this.configuracionReqMinService
      .getDeclaracionJuradaByPerfil(this.idPerfil, this.idBase)
      .subscribe(
        (mc) => {
          console.log('MC', mc);

          this.lstDDJJGeneral = mc.filter((obj) => obj.isServir === '1');
          this.lstDDJJEspecifica = mc.filter((obj) => obj.isServir === '0');

          if (this.lstDDJJGeneral === null) {
            this.lstDDJJGeneral = [];
          }

          if (this.lstDDJJEspecifica === null) {
            this.lstDDJJEspecifica = [];
          }
        },
        (error) => {
          console.log('Error', error);
        }
      );
  }

  fillLeft(number, width) {
    let numberOutput = Math.abs(number);
    let length = number.toString().length;
    let zero = '0';

    if (width <= length) {
      if (number < 0) {
        return '-' + numberOutput.toString();
      } else {
        return numberOutput.toString();
      }
    } else {
      if (number < 0) {
        return '-' + zero.repeat(width - length) + numberOutput.toString();
      } else {
        return zero.repeat(width - length) + numberOutput.toString();
      }
    }
  }

  onSelectRadioDDJJGeneral(e, index: number) {
    this.lstDDJJGeneral[index].flagDDJJ = Number(e.value);
    this.validarBtnContinuar(3);
  }

  onSelectRadioDDJJEspecifica(e, index: number) {
    this.lstDDJJEspecifica[index].flagDDJJ = Number(e.value);
    this.validarBtnContinuar(3);
  }

  onCheckReqMinDDJJGeneral(e, index: number): void {
    this.lstDDJJGeneral[index].requisitoMinimo = e;
    this.validarBtnContinuar(3);
  }

  onCheckReqMinDDJJEspecifica(e, index: number): void {
    this.lstDDJJEspecifica[index].requisitoMinimo = e;
    this.validarBtnContinuar(3);
  }

  /* MÉTODOS GENÉRICOS */
  calcularTotalPuntos() {
    this.resumen.puntosMin = Utils.rountTo(
      this.puntosMinFormacionAcademica +
        this.puntosMinExpLabGeneral +
        this.puntosMinExpLabEspecifica,
      2
    );
  }

  calcularPuntajeMinimoFA() {
    let puntajeCarrera: number = 0;
    let puntajeProgramas: number = 0;
    let puntajeCursos: number = 0;

    this.lstCarreras.forEach((element) => {
      if (element.flagReqMin) {
        puntajeCarrera = element.puntaje != null ? Number(element.puntaje) : 0;
      }
    });

    this.lstEspecificacionPrograma.forEach((element) => {
      if (element.requisitoMinimo) {
        puntajeProgramas = Number(element.puntaje);
      }
    });

    this.lstEspecificacionCurso.forEach((element) => {
      if (element.requisitoMinimo) {
        puntajeCursos = Number(element.puntaje);
      }
    });

    if (this.pesoFAExpGeneral !== null && this.pesoFAExpGeneral !== 0) {
      this.puntosMinFormacionAcademica =
        (puntajeCarrera + puntajeProgramas + puntajeCursos) *
        (this.pesoFAExpGeneral / 100);
      this.calcularTotalPuntos();
    }
  }

  calcularPuntajeMinimoELGeneral() {
    let puntajeExpGeneral: number = 0;
    if (this.lstExperienciaGeneral != null) {
      this.lstExperienciaGeneral.forEach((element) => {
        if (element.listDetalleExperiencia != null) {
          element.listDetalleExperiencia.forEach((det) => {
            if (det.requisitoMinimo) {
              puntajeExpGeneral = det.puntaje != null ? Number(det.puntaje) : 0;
            }
          });
        }
      });
    }
    if (this.pesoELExpGeneral !== null && this.pesoELExpGeneral !== 0) {
      this.puntosMinExpLabGeneral =
        puntajeExpGeneral * (this.pesoELExpGeneral / 100);
      this.calcularTotalPuntos();
    }
    if (isNaN(this.pesoELExpGeneral)) {
      this.puntosMinExpLabGeneral = 0;
    }
  }

  calcularPuntajeMinimoELEspecifico() {
    let puntajeExpEspecificaMateria: number = 0;
    let puntajeExpEspecificaPublico: number = 0;
    let puntajeExpJefatural: number = 0;

    if (this.lstExpEspecificaMateria != null) {
      this.lstExpEspecificaMateria.forEach((element) => {
        if (element.listDetalleExperiencia != null) {
          element.listDetalleExperiencia.forEach((det) => {
            if (det.requisitoMinimo) {
              puntajeExpEspecificaMateria =
                det.puntaje != null ? Number(det.puntaje) : 0;
            }
          });
        }
      });
    }

    if (this.lstExpEspecificaPublico != null) {
      this.lstExpEspecificaPublico.forEach((element) => {
        if (element.listDetalleExperiencia != null) {
          element.listDetalleExperiencia.forEach((det) => {
            if (det.requisitoMinimo) {
              puntajeExpEspecificaPublico =
                det.puntaje != null ? Number(det.puntaje) : 0;
            }
          });
        }
      });
    }

    if (this.lstExpJefatural != null) {
      this.lstExpJefatural.forEach((element) => {
        if (element.listDetalleExperiencia != null) {
          element.listDetalleExperiencia.forEach((det) => {
            if (det.requisitoMinimo) {
              puntajeExpJefatural =
                det.puntaje != null ? Number(det.puntaje) : 0;
            }
          });
        }
      });
    }

    if (this.pesoELExpEspecifica !== null && this.pesoELExpEspecifica !== 0) {
      this.puntosMinExpLabEspecifica =
        (puntajeExpEspecificaMateria +
          puntajeExpEspecificaPublico +
          puntajeExpJefatural) *
        (this.pesoELExpEspecifica / 100);
      this.calcularTotalPuntos();
    }
  }

  calcularPuntajeMaximoFA() {
    let puntosExpGeneral =
      Math.max.apply(
        Math,
        this.lstCarreras.map(function (item) {
          return item.puntaje;
        })
      ) *
      (this.pesoFAExpGeneral / 100);

    let puntosProgramas = 0;
    if (
      this.lstEspecificacionPrograma !== null &&
      this.lstEspecificacionPrograma.length > 0
    ) {
      puntosProgramas =
        Math.max.apply(
          Math,
          this.lstEspecificacionPrograma.map(function (item) {
            return item.puntaje;
          })
        ) *
        (this.pesoFAExpGeneral / 100);
    }

    let puntosCursos = 0;
    if (
      this.lstEspecificacionCurso !== null &&
      this.lstEspecificacionCurso.length > 0
    ) {
      puntosCursos =
        Math.max.apply(
          Math,
          this.lstEspecificacionCurso.map(function (item) {
            return item.puntaje;
          })
        ) *
        (this.pesoFAExpGeneral / 100);
    }
    this.puntosMaxFormacionAcademica =
      puntosExpGeneral + puntosProgramas + puntosCursos;
    this.calcularTotalPuntosMaximos();
  }

  calcularPuntajeMaximoELGeneral() {
    let puntosMaxGeneral = 0;
    if (
      this.lstExperienciaGeneral[0] !== null &&
      this.lstExperienciaGeneral[0].listDetalleExperiencia !== null
    ) {
      puntosMaxGeneral =
        Math.max.apply(
          Math,
          this.lstExperienciaGeneral[0].listDetalleExperiencia.map(function (
            item
          ) {
            return item.puntaje;
          })
        ) *
        (this.pesoELExpGeneral / 100);
    }
    this.puntosMaxExpLabGeneral = Number(isNaN(puntosMaxGeneral) ? 0 : puntosMaxGeneral);
    this.calcularTotalPuntosMaximos();
  }

  calcularPuntajeMaximoELEspecifica() {
    let puntosMaxMateria = 0;
    if (
      this.lstExpEspecificaMateria[0] !== null &&
      this.lstExpEspecificaMateria.length > 0 &&
      this.lstExpEspecificaMateria[0].listDetalleExperiencia !== null &&
      this.lstExpEspecificaMateria[0].listDetalleExperiencia.length > 0
    ) {
      puntosMaxMateria =
        Math.max.apply(
          Math,
          this.lstExpEspecificaMateria[0].listDetalleExperiencia.map(function (
            item
          ) {
            return item.puntaje;
          })
        ) *
        (this.pesoELExpEspecifica / 100);
    }

    let puntosMaxPublica = 0;
    if (
      this.lstExpEspecificaPublico[0] !== null &&
      this.lstExpEspecificaPublico.length > 0 &&
      this.lstExpEspecificaPublico[0].listDetalleExperiencia !== null &&
      this.lstExpEspecificaPublico[0].listDetalleExperiencia.length > 0
    ) {
      puntosMaxPublica =
        Math.max.apply(
          Math,
          this.lstExpEspecificaPublico[0].listDetalleExperiencia.map(function (
            item
          ) {
            return item.puntaje;
          })
        ) *
        (this.pesoELExpEspecifica / 100);
    }

    let puntosMaxJefatural = 0;
    if (
      this.lstExpJefatural[0] !== null &&
      this.lstExpJefatural.length > 0 &&
      this.lstExpJefatural[0].listDetalleExperiencia !== null &&
      this.lstExpJefatural[0].listDetalleExperiencia.length > 0
    ) {
      puntosMaxJefatural =
        Math.max.apply(
          Math,
          this.lstExpJefatural[0].listDetalleExperiencia.map(function (item) {
            return item.puntaje;
          })
        ) *
        (this.pesoELExpEspecifica / 100);
    }

    this.puntosMaxExpLabEspecifica =
      puntosMaxMateria + puntosMaxPublica + puntosMaxJefatural;
    this.calcularTotalPuntosMaximos();
  }

  calcularTotalPuntosMaximos() {
    this.resumen.puntosMax = Utils.rountTo(
      (this.puntosMaxFormacionAcademica === null
        ? 0
        : this.puntosMaxFormacionAcademica) +
        (this.puntosMaxExpLabGeneral === null
          ? 0
          : this.puntosMaxExpLabGeneral) +
        (this.puntosMaxExpLabEspecifica === null
          ? 0
          : this.puntosMaxExpLabEspecifica),
      2
    );

    console.log('puntosMax: ', this.resumen.puntosMax);

    if (isNaN(this.resumen.puntosMax)) {
      this.resumen.puntosMax = 0;
    }

    if (Math.sign(this.resumen.puntosMax) <= -1) {
      this.resumen.puntosMax = 0;
    }
  }

  atras(): void {
    if (this.stepper.selectedIndex > 0) {
      this.stepper.previous();
    }
    this.showHideContenidoStepper();
  }

  continuar(): void {
    console.log(this.stepper.selectedIndex);
    if (this.stepper.selectedIndex < this.stepper.steps.length - 1) {
      if (this.stepper.selectedIndex === 1) {
        // validamos que el Total % max sea 100
        this.calcularResumenTotal();
        if (this.resumen.total === 100) {
          this.stepper.next();
        } else {
          this.toast.showToast(
            'El total de peso debe sumar 100',
            'danger',
            'Advertencia'
          );
        }
      } else {
        // Para cualquier step
        this.stepper.next();
      }

      this.showHideContenidoStepper();
    } else {
      console.log('Guardar');
      this.continuarGuardar();
    }
  }

  continuarGuardar(): void {
    if (this.validarGuardar()) {
      let listaConfigPerfPesoSeccion: any[] = [];
      listaConfigPerfPesoSeccion = this.getConfigPerfPesoSeccion();

      let listConfigPerfFormacEspecif = [];
      listConfigPerfFormacEspecif = this.getConfigPerfFormacEspecif();

      let listConfigPerfExpeLaboral = [];
      listConfigPerfExpeLaboral = this.getConfigPerfExpeLaboral();

      let listConfigPerfDeclaraJurada = [];
      listConfigPerfDeclaraJurada = this.getConfigPerfDeclaradaJurada();

      let listConfigPerfFormacCarrera = [];
      listConfigPerfFormacCarrera = this.getConfigPerfFormacCarrera();

      let listConfigPerfFormacEspecial = [];
      listConfigPerfFormacEspecial = this.getConfigPerfFormacEspecial();

      let listConfigPerfOtrosRequisitos = [];
      listConfigPerfOtrosRequisitos = this.getConfigPerfOtrosRequisitos();

      let listaConfigOtrosGrados = [];
      listaConfigOtrosGrados = this.lstGradosFormacionACarrera;

      let configPerfiles = this.getConfiguracionPerfil();

      let configuracionPerfilDTO: any;
      configuracionPerfilDTO = {
        listConfigPerfFormacCarrera: listConfigPerfFormacCarrera,
        listConfigPerfFormacEspecial: listConfigPerfFormacEspecial,
        listConfigPerfFormacEspecif: listConfigPerfFormacEspecif,
        listConfigPerfExpeLaboral: listConfigPerfExpeLaboral,
        listConfigPerfOtrosRequisitos: listConfigPerfOtrosRequisitos,
        listConfigPerfDeclaraJurada: listConfigPerfDeclaraJurada,
        listaConfigPerfPesoSeccion: listaConfigPerfPesoSeccion,
        listaConfigOtrosGrados: listaConfigOtrosGrados,
        listaConfigInvestigacion: this.listaConfigInvestigacion,
        listaConfigPublicacion: this.listaConfigPublicacion,
        listaConfigEspecifInvestigacion: this.listaConfigEspecifInvestigacion,
        listaConfigEspecifPublicacion: this.listaConfigEspecifPublicacion,

        configPerfiles: configPerfiles,
      };

      this.matDialog.open(DialogGuardarConfiguracionPerfilComponent, {
        data: configuracionPerfilDTO,
        width: '500px',
      });
    }
  }

  getConfiguracionPerfil() {
    let configPerfiles = {
      configPerfilId: this.selectedConfigPerfilId,
      perfilId: this.idPerfil,
      tipoPerfil: this.selectedPlantilla,
      baseId: this.idBase,
    };
    return configPerfiles;
  }

  getConfigPerfOtrosRequisitos() {
    let listConfigPerfOtrosRequisitos = [];
    this.lstOtrosRequisitos.forEach((element) => {
      let objOtrosRequisitos: any;
      objOtrosRequisitos = {
        confPerfOtrosRequisitosId: element.confPerfOtrosRequisitosId,
        perfilId: this.idPerfil,
        descripcion: element.descripcion,
        requisitosAdicionales: element.requisitoMinimo,
        experienciaDetalleId: element.experienciaDetalleId,
        tipoDatoExperId: element.tipoDatoExperId,
        requisitoId: element.requisitoId,
        perfilExperienciaId: element.perfilExperienciaId,
        baseId: this.idBase,
      };
      listConfigPerfOtrosRequisitos.push(objOtrosRequisitos);
    });
    return listConfigPerfOtrosRequisitos;
  }

  getConfigPerfFormacEspecial() {
    let listConfigPerfFormacEspecial = [];
    this.lstEspecialidades.forEach((element) => {
      let objEspecialidades: any;
      objEspecialidades = {
        confPerfFormacEspecialId: element.confPerfFormacEspecialId,
        perfilId: this.idPerfil,
        tipoConocimientoId: element.tipoConocimientoId,
        descripcionTipo: element.descTipo,

        conocimientoId: element.conocimientoId,
        descripcionConocimiento: element.descConocimiento,
        requisitoMinimo: element.flagReqMin,
        puntaje: element.puntaje,
      };
      listConfigPerfFormacEspecial.push(objEspecialidades);
    });

    return listConfigPerfFormacEspecial;
  }

  getConfigPerfFormacCarrera() {
    let listConfigPerfFormacCarrera = [];
    this.lstCarreras.forEach((element) => {
      let objCarrera: any;
      objCarrera = {
        confPerfFormacCarreraId: element.carreraId,
        perfilId: this.idPerfil,
        nivelEducativoId: element.nivelEducativoId,
        descripcionAcademica: element.descAcademica,
        descripcionNivelEducativo: element.descNivelEducativo,
        requisitoMinimo: element.flagReqMin,
        puntaje: element.puntaje,
        situacionAcademicaId: element.situacionAcademicaId,
        descripcionCarrera: element.descCarreras,
        baseId: this.idBase,
        carreraIds: element.carreraIds,
      };

      listConfigPerfFormacCarrera.push(objCarrera);
    });

    return listConfigPerfFormacCarrera;
  }

  getConfigPerfDeclaradaJurada() {
    let listConfigPerfDeclaraJurada = [];

    if (this.lstDDJJGeneral != null && this.lstDDJJGeneral.length > 0) {
      this.lstDDJJGeneral.forEach((element) => {
        let objDDJJ: any;
        objDDJJ = {
          confPerfDeclaracionId: element.confPerfDeclaracionId,
          perfilId: this.idPerfil,
          descripcion: element.descripcion,
          requisitoMinimo: element.requisitoMinimo,
          flagDDJJ: element.flagDDJJ,
          declaracionId: element.declaracionId,
          tipoId: element.tipoId,
          orden: element.orden,
          estado: element.estado,
          isServir: element.isServir,
          baseId: this.idBase,
        };
        listConfigPerfDeclaraJurada.push(objDDJJ);
      });
    }

    if (this.lstDDJJEspecifica != null && this.lstDDJJEspecifica.length > 0) {
      this.lstDDJJEspecifica.forEach((element) => {
        let objDDJJ: any;
        objDDJJ = {
          confPerfDeclaracionId: element.confPerfDeclaracionId,
          perfilId: this.idPerfil,
          descripcion: element.descripcion,
          requisitoMinimo: element.requisitoMinimo,
          flagDDJJ: element.flagDDJJ,
          declaracionId: element.declaracionId,
          tipoId: element.tipoId,
          orden: element.orden,
          estado: element.estado,
          isServir: element.isServir,
          baseId: this.idBase,
        };
        listConfigPerfDeclaraJurada.push(objDDJJ);
      });
    }

    return listConfigPerfDeclaraJurada;
  }

  validarGuardar(): boolean {
    let result: boolean = true;
    let mensaje = '';
    let reqMinValid: boolean = false;

    if (this.pesoFAExpGeneral === null || this.pesoFAExpGeneral === 0) {
      mensaje = 'Ingrese un peso en Formación Académica';
      result = false;
    } else if (this.pesoELExpGeneral === null || this.pesoELExpGeneral === 0) {
      mensaje = 'Ingrese un peso en Experiencia Laboral - General';
      result = false;
    } else if (
      this.pesoELExpEspecifica === null ||
      this.pesoELExpEspecifica === 0
    ) {
      mensaje = 'Ingrese un peso en Experiencia Laboral - EFguspecífica';
      result = false;
    }

    if (this.lstDDJJGeneral.length > 0) {
      this.lstDDJJGeneral.forEach((element) => {
        if (element.flagDDJJ === null) {
          result = false;
          mensaje =
            'Debe responder las declaraciones juradas generales para continuar';
          return result;
        }
        if (element.requisitoMinimo) {
          reqMinValid = true;
        }
      });
    } else {
      result = true;
    }

    if (this.lstDDJJEspecifica.length > 0) {
      this.lstDDJJEspecifica.forEach((element) => {
        if (element.flagDDJJ === null) {
          result = false;
          mensaje =
            'Debe responder las declaraciones juradas específicas para continuar';
          return result;
        }

        if (element.requisitoMinimo) {
          reqMinValid = true;
        }
      });
    } else {
      result = true;
    }

    if (!result) {
      result = false;
      this.toast.showToast(
        'Marque al menos un requisito mínimo en cada sección',
        'warning',
        'Atención'
      );
    }

    if (mensaje !== '') {
      this.toast.showToast(mensaje, 'warning', 'Atención');
    }

    return result;
  }

  getConfigPerfPesoSeccion() {
    let listaConfigPerfPesoSeccion: any[] = [];
    let peso: number;
    let tipo: number;

    if (this.lstPesos !== null && this.lstPesos.length > 0) {
      this.lstPesos.forEach((element) => {
        switch (element.tipoSeccion) {
          case 0:
            element.pesoSeccion = this.pesoFAExpGeneral;
            break;
          case 1:
            element.pesoSeccion = this.pesoELExpGeneral;
            break;
          case 2:
            element.pesoSeccion = this.pesoELExpEspecifica;
            break;
          case 3:
            element.pesoSeccion = this.pesoELInvestigacion;
            break;
          case 4:
            element.pesoSeccion = this.pesoELExpJefatural;
            break;
          // case 5:
          //   element.pesoSeccion = this.pesoELInvestigacion;
          //     break;
          default:
            element.pesoSeccion = 0;
            break;
        }
        const obj = {
          confPerfSeccionId: element.confPerfSeccionId,
          idPerfil: this.idPerfil,
          tipoSeccion: element.tipoSeccion,
          pesoSeccion: element.pesoSeccion,
          descripcionSeccion: element.descripcionSeccion,
          baseId: this.idBase,
        };

        listaConfigPerfPesoSeccion.push(obj);
      });
    } else {
      for (let i = 0; i < this.cantidadPesos - 1; i++) {
        let descTipo = '';
        switch (i) {
          case 0:
            tipo = i;
            peso = this.pesoFAExpGeneral;
            descTipo = 'Peso Formacion Academica General';
            break;
          case 1:
            tipo = i;
            peso = this.pesoELExpGeneral;
            descTipo = 'Peso Experiencia Laboral General';
            break;
          case 2:
            tipo = i;
            peso = this.pesoELExpEspecifica;
            descTipo = 'Peso Experiencia Específica';
            break;
          case 3:
            tipo = i;
            peso = this.pesoELExpJefatural;
            descTipo = 'Peso Experiencia Jefatural';
            break;
          case 4:
            tipo = i;
            peso = this.pesoELInvestigacion;
            descTipo = 'Peso Especificación de Investigación y Publicación';
            break;
          default:
            tipo = i;
            peso = 0;
            descTipo = 'Peso no identificado';
            break;
        }
        const element = {
          confPerfSeccionId: null,
          idPerfil: this.idPerfil,
          tipoSeccion: tipo,
          pesoSeccion: peso,
          descripcionSeccion: descTipo,
          baseId: this.idBase,
        };

        listaConfigPerfPesoSeccion.push(element);
      }
    }
    console.log('Lista de pesos REQUEST', listaConfigPerfPesoSeccion);
    return listaConfigPerfPesoSeccion;
  }

  getConfigPerfFormacEspecif() {
    let listConfigPerfFormacEspecif = [];

    this.lstEspecificacionPrograma.forEach((element) => {
      let objEspecificacion = {
        confPerfFormacEspecifId: element.especificacionId,
        perfilId: element.perfilId,
        tipoEspecificacion: 162,
        requisitoMinimo: element.requisitoMinimo,
        cantidad: element.cantidad,
        puntaje: element.puntaje,
        descripcion: element.descripcion,
        baseId: this.idBase,
      };
      listConfigPerfFormacEspecif.push(objEspecificacion);
    });

    this.lstEspecificacionCurso.forEach((element) => {
      let objEspecificacion = {
        confPerfFormacEspecifId: element.especificacionId,
        perfilId: element.perfilId,
        tipoEspecificacion: 161,
        requisitoMinimo: element.requisitoMinimo,
        cantidad: element.cantidad,
        puntaje: element.puntaje,
        descripcion: element.descripcion,
        baseId: this.idBase,
      };
      listConfigPerfFormacEspecif.push(objEspecificacion);
    });

    return listConfigPerfFormacEspecif;
  }

  getConfigPerfExpeLaboral() {
    let listConfigPerfExpeLaboral = [];
    this.lstExperienciaGeneral.forEach((element) => {
      if (element.listDetalleExperiencia != null) {
        element.listDetalleExperiencia.forEach((det) => {
          let objExperiencia = {
            confPerfExpeLaboralId: det.confPerfExpeLaboralId,
            perfilId: element.perfilId,
            tipoExperiencia: element.tipoExperiencia,
            nivelAcademico: element.descripcionNivel,
            nivelEducativoId: element.nivelEducativoId,
            nivelEducativoDesc: element.nivelEducativoDesc,
            situacionAcademicaId: element.situacionAcademicaId,
            situacionAacademicaDesc: element.sitAcademiDesc,
            desdeAnios: det.desdeAnios,
            hastaAnios: det.hastaAnios,
            requisitoMinimo: det.requisitoMinimo,
            puntaje: det.puntaje,
            baseId: this.idBase,
            listModeloDetalleExperiencia: element.listModeloDetalleExperiencia,
          };
          listConfigPerfExpeLaboral.push(objExperiencia);
        });
      }
    });

    this.lstExpEspecificaMateria.forEach((element) => {
      if (element.listDetalleExperiencia != null) {
        element.listDetalleExperiencia.forEach((det) => {
          let objExperiencia = {
            confPerfExpeLaboralId: det.confPerfExpeLaboralId,
            perfilId: element.perfilId,
            tipoExperiencia: element.tipoExperiencia,
            nivelAcademico: element.descripcionNivel,

            nivelEducativoId: element.nivelEducativoId,
            nivelEducativoDesc: element.nivelEducativoDesc,
            situacionAcademicaId: element.situacionAcademicaId,
            situacionAacademicaDesc: element.sitAcademiDesc,
            desdeAnios: det.desdeAnios,
            hastaAnios: det.hastaAnios,
            requisitoMinimo: det.requisitoMinimo,
            puntaje: det.puntaje,
            baseId: this.idBase,
            listModeloDetalleExperiencia: element.listModeloDetalleExperiencia,
          };
          listConfigPerfExpeLaboral.push(objExperiencia);
        });
      }
    });

    this.lstExpEspecificaPublico.forEach((element) => {
      if (element.listDetalleExperiencia != null) {
        element.listDetalleExperiencia.forEach((det) => {
          let objExperiencia = {
            confPerfExpeLaboralId: det.confPerfExpeLaboralId,
            perfilId: element.perfilId,
            tipoExperiencia: element.tipoExperiencia,
            nivelAcademico: element.descripcionNivel,
            nivelEducativoId: element.nivelEducativoId,
            nivelEducativoDesc: element.nivelEducativoDesc,
            situacionAcademicaId: element.situacionAcademicaId,
            situacionAacademicaDesc: element.sitAcademiDesc,
            desdeAnios: det.desdeAnios,
            hastaAnios: det.hastaAnios,
            requisitoMinimo: det.requisitoMinimo,
            puntaje: det.puntaje,
            baseId: this.idBase,
            listModeloDetalleExperiencia: element.listModeloDetalleExperiencia,
          };
          listConfigPerfExpeLaboral.push(objExperiencia);
        });
      }
    });

    this.lstExpJefatural.forEach((element) => {
      if (element.listDetalleExperiencia != null) {
        element.listDetalleExperiencia.forEach((det) => {
          let objExperiencia = {
            confPerfExpeLaboralId: det.confPerfExpeLaboralId,
            perfilId: element.perfilId,
            tipoExperiencia: element.tipoExperiencia,
            nivelAcademico: element.descripcionNivel,
            nivelEducativoId: element.nivelEducativoId,
            nivelEducativoDesc: element.nivelEducativoDesc,
            situacionAcademicaId: element.situacionAcademicaId,
            situacionAacademicaDesc: element.sitAcademiDesc,
            desdeAnios: det.desdeAnios,
            hastaAnios: det.hastaAnios,
            requisitoMinimo: det.requisitoMinimo,
            puntaje: det.puntaje,
            baseId: this.idBase,
          };
          listConfigPerfExpeLaboral.push(objExperiencia);
        });
      }
    });

    this.lstExpNivelEspecifico.forEach((element) => {
      if (element.listDetalleExperiencia != null) {
        element.listDetalleExperiencia.forEach((det) => {
          let objExperiencia = {
            confPerfExpeLaboralId: det.confPerfExpeLaboralId,
            perfilId: element.perfilId,
            tipoExperiencia: element.tipoExperiencia,
            nivelAcademico: element.descripcionNivel,
            nivelEducativoId: element.nivelEducativoId,
            nivelEducativoDesc: element.nivelEducativoDesc,
            situacionAcademicaId: element.situacionAcademicaId,
            situacionAacademicaDesc: element.sitAcademiDesc,
            desdeAnios: det.desdeAnios,
            hastaAnios: det.hastaAnios,
            requisitoMinimo: det.requisitoMinimo,
            puntaje: det.puntaje,
            baseId: this.idBase,
            listModeloDetalleExperiencia: element.listModeloDetalleExperiencia,
          };
          listConfigPerfExpeLaboral.push(objExperiencia);
        });
      }
    });

    return listConfigPerfExpeLaboral;
  }

  showHideContenidoStepper(): void {
    switch (this.stepper.selectedIndex) {
      case 0:
        this.isStepFormacion = true;
        this.isStepExperiencia = false;
        this.isStepOtrosReq = false;
        this.isStepDeclaracionJur = false;
        this.isGuardarEnabled = false;
        this.isVolverEnabled = false;
        this.validarBtnContinuar(this.stepper.selectedIndex);
        break;
      case 1:
        this.isStepFormacion = false;
        this.isStepExperiencia = true;
        this.isStepOtrosReq = false;
        this.isStepDeclaracionJur = false;
        this.isGuardarEnabled = false;
        this.isVolverEnabled = true;
        this.validarBtnContinuar(this.stepper.selectedIndex);
        break;
      case 2:
        this.isStepFormacion = false;
        this.isStepExperiencia = false;
        this.isStepOtrosReq = true;
        this.isStepDeclaracionJur = false;
        this.isGuardarEnabled = false;
        this.isVolverEnabled = true;
        this.validarBtnContinuar(this.stepper.selectedIndex);
        break;
      case 3:
        this.isStepFormacion = false;
        this.isStepExperiencia = false;
        this.isStepOtrosReq = false;
        this.isStepDeclaracionJur = true;
        this.isContinuarEnabled = false;
        this.isVolverEnabled = true;
        this.validarBtnContinuar(this.stepper.selectedIndex);
        break;
    }
  }

  validarBtnContinuar(i: number): void {
    this.isContinuarEnabled = false;
    switch (i) {
      case 0:
        if (this.lstCarreras != null && this.lstCarreras.length > 0) {
          const reqMinCarreras = this.lstCarreras.find(
            (element) => element.flagReqMin === true
          );

          if (reqMinCarreras && this.pesoFAExpGeneral > 0) {
            const puntajeCarrera = reqMinCarreras.puntaje;
            if (puntajeCarrera > 0) {
              this.isContinuarEnabled = true;
            }
          }
        }
        break;
      case 1:
        if (
          this.lstExperienciaGeneral != null &&
          this.lstExperienciaGeneral.length > 0 &&
          this.lstExperienciaGeneral[0].listDetalleExperiencia != null &&
          this.lstExperienciaGeneral[0].listDetalleExperiencia.length > 0 &&
          this.lstExpEspecificaMateria != null &&
          this.lstExpEspecificaMateria.length > 0 &&
          this.lstExpEspecificaPublico != null &&
          this.lstExpEspecificaPublico.length > 0 &&
          this.lstExpNivelEspecifico != null &&
          this.lstExpNivelEspecifico.length > 0
        ) {
          const reqMinGeneral = this.lstExperienciaGeneral[0].listDetalleExperiencia.find(
            (element) => element.requisitoMinimo === true
          );

          const reqMinEspecificaMateria = this.lstExpEspecificaMateria[0].listDetalleExperiencia.find(
            (element) => element.requisitoMinimo === true
          );

          const reqMinEspecificaPublico = this.lstExpEspecificaPublico[0].listDetalleExperiencia.find(
            (element) => element.requisitoMinimo === true
          );

          const reqMinNivelEspecifico = this.lstExpNivelEspecifico[0].listDetalleExperiencia.find(
            (element) => element.requisitoMinimo === true
          );

          if (
            reqMinGeneral &&
            reqMinEspecificaMateria &&
            reqMinEspecificaPublico &&
            this.pesoELExpGeneral > 0
          ) {
            this.isContinuarEnabled = true;
          }
        }
        break;
      case 2:
        // if (
        //   this.lstOtrosRequisitos != null &&
        //   this.lstOtrosRequisitos.length > 0
        // ) {
        //   const reqMinOtrosReq = this.lstOtrosRequisitos.find(
        //     (element) => element.requisitoMinimo === true
        //   );
        //   if (reqMinOtrosReq) {
        //     this.isContinuarEnabled = true;
        //   }
        // }
        this.isContinuarEnabled = true;
        break;
      case 3:
        if (this.lstDDJJGeneral.length === 0) {
          this.isGuardarEnabled = true;
        } else if (
          this.lstDDJJGeneral != null &&
          this.lstDDJJGeneral.length > 0 &&
          this.lstDDJJEspecifica.length > 0 &&
          this.lstDDJJEspecifica != null
        ) {
          const reqMinGeneral = this.lstDDJJGeneral.find(
            (element) => element.requisitoMinimo === true
          );
          const reqMinEspecifica = this.lstDDJJEspecifica.find(
            (element) => element.requisitoMinimo === true
          );

          const flagDDJJGeneral = this.lstDDJJGeneral.find(
            (element) => element.flagDDJJ === null
          );
          const flagDDJJEspecifica = this.lstDDJJGeneral.find(
            (element) => element.flagDDJJ === null
          );
          if (
            reqMinGeneral &&
            reqMinEspecifica &&
            !flagDDJJGeneral &&
            !flagDDJJEspecifica
          ) {
            this.isGuardarEnabled = true;
          }
        }

        break;
      default:
        this.isContinuarEnabled = false;
        break;
    }
  }

  backConfiguracion(): void {
    localStorage.removeItem('selectedPlantilla');
    localStorage.removeItem('selectedconfigPerfilId');
    this.router.navigateByUrl('/pages/configuracion-evaluacion');
  }

  actualizarResumen(e, i: number): void {
    if (Number(e.target.value)) {
      switch (i) {
        case 1:
          this.pesoFAExpGeneral = Number(e.target.value);
          this.validarBtnContinuar(0);
          break;
        case 2:
          this.pesoELExpGeneral = Number(e.target.value);
          this.validarBtnContinuar(1);
          break;
        case 3:
          this.pesoELExpEspecifica = Number(e.target.value);
          this.validarBtnContinuar(1);
          break;
        case 4:
          this.pesoELExpJefatural = Number(e.target.value);
          this.validarBtnContinuar(1);
          break;
        case 5:
          this.pesoELInvestigacion = Number(e.target.value);
          this.validarBtnContinuar(1);
          break;
        default:
          break;
      }

      this.calcularResumenTotal();
    }
    this.calcularPuntajeMinimoFA();
    this.calcularPuntajeMaximoFA();
    this.calcularPuntajeMinimoELGeneral();
    this.calcularPuntajeMinimoELEspecifico();
    this.calcularPuntajeMaximoELGeneral();
    this.calcularPuntajeMaximoELEspecifica();
  }

  calcularResumenTotal() {
    this.resumen.total =
      this.pesoFAExpGeneral +
      this.pesoELExpGeneral +
      this.pesoELExpEspecifica +
      this.pesoELExpJefatural +
      this.pesoELInvestigacion;
  }

  keyPressNumbers(event) {
    const charCode = event.which ? event.which : event.keyCode;
    if (charCode < 48 || charCode > 57) {
      event.preventDefault();
      return false;
    } else {
      return true;
    }
  }
}
