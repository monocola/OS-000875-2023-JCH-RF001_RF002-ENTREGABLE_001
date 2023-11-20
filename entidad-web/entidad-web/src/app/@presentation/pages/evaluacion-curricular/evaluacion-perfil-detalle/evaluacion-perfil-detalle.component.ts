import { ToastService } from './../../../@common-components/toast';
import { forkJoin } from 'rxjs';
import { Const } from 'src/app/@data/services/const';
import { Component, OnInit, ViewChild } from '@angular/core';

import { ModalAlertConfirmComponent } from '../modal-alert-confirm/modal-alert-confirm.component';
import { ModalVerDocumentoComponent } from '../modal-ver-documento/modal-ver-documento.component';
import { MatDialog } from '@angular/material/dialog';
import { EvaluacionCurricularRepository } from 'src/app/@domain/repository/evaluacion-curricular.repository';
import { MatStepper } from '@angular/material/stepper';
import { Router } from '@angular/router';
import { ModalBonificacionConfirmComponent } from '../modal-bonificacion-confirm/modal-bonificacion-confirm.component';
import { STEPPER_GLOBAL_OPTIONS } from '@angular/cdk/stepper';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';

@Component({
  selector: 'serv-talento-evaluacion-perfil-detalle',
  templateUrl: './evaluacion-perfil-detalle.component.html',
  styleUrls: ['./evaluacion-perfil-detalle.component.scss'],
  providers: [
    {
      provide: STEPPER_GLOBAL_OPTIONS,
      useValue: { displayDefaultIndicatorType: true },
    },
  ],
})
export class EvaluacionPerfilDetalleComponent implements OnInit {
  data: any;
  convocatoriaId: number;
  postulanteId: number;
  perfilId: number;
  convocatoriaPostulanteId: number;
  urlFFAA: string;
  urlDeportista: string;
  urlConadi: string;
  tipoFFAA: string[] = [];
  tipoConadi: string[] = [];
  tipoDeportista: string[] = [];

  selectedIndex: number = 0;
  puntajes: any[] = [];
  puntajeInvestigacion: number = 0;
  puntajePublicacion: number = 0;
  cantMarcadosInvest: number = 0;
  cantMarcadosPublic: number = 0;

  pesoELInvestigacion: number = 0;

  resumenTotal: any;
  listaExperienciaByPerfil: any[] = [];
  listaInvestigacionByPerfil: any;
  listarFormacionByPerfil: any;
  listarCarrerasPerfil: any[] = [];
  listarEspecialidadesPerfil: any[] = [];
  listarExperienciaPostulante: any[] = [];
  lstInvestigacionPostulante: any = {};
  listaOtrosRequisitoPostulante: any[] = [];
  listarConfiguracionOtrosReque: any[] = [];
  lstGradosFormacionACarrera: any[] = [];
  ltaDDJJPostulanteServir: any[] = [];
  indicadorPostulante: any;

  lstBonoDeportista: any[] = [];
  lstBonoFFAA: any[] = [];
  lstBonoConadi: any[] = [];
  lstPesos: any;
  lstDDJJEntidadServir: any[];
  lstDDJJEntidadEspecifica: any[];

  @ViewChild('stepper') private stepper: MatStepper;

  constructor(
    private dialog: MatDialog,
    private evaluacionCurricularService: EvaluacionCurricularRepository,
    private maestraService: MaestraService,
    private configuracionReqMinService: ConfiguracionReqMinRepository,
    private router: Router,
    private toast: ToastService
  ) { }

  ngOnInit(): void {
    this.data = JSON.parse(
      localStorage.getItem('convocatoriaPostulanteSelected')
    );
    this.convocatoriaId = this.data.idBase;
    this.perfilId = this.data.idPerfil;
    this.postulanteId = this.data.postulanteId;
    this.convocatoriaPostulanteId = this.data.convocatoriaPostulanteId;
    this.getData();

  }

  getData() {
    this.cargarPesosConfiguracion();
    this.listarFormacionAcademica();
    this.listarExperiencia();
    this.listarInvestigacionPostulante();
    this.listarDeclaracionesJuradas();
    this.listarPuntajes();
    this.listarOtrosRequisitosPostulante();
    this.ListaConfigOtrosrequisitos();
    this.listarResumen();
    this.listarUrlDocumentos();
    this.listarBonoConvocatoria(this.convocatoriaPostulanteId);
    this.obtenerTipoConfiguracion();

  }

  cargarPesosConfiguracion() {
    this.configuracionReqMinService.getPesosConfiguracion(this.perfilId, this.convocatoriaId).subscribe((mc) => {
      if (mc.items != null) {
        this.lstPesos = mc.items;
        mc.items.forEach(element => {
          switch (element.tipoSeccion) {

            case 3:
              this.pesoELInvestigacion = element.pesoSeccion;
              break;

            default:
              break;
          }
        });

      }

    });

  }

  obtenerTipoConfiguracion() {
    this.configuracionReqMinService
      .obtenerPlantillaPorPerfil(this.perfilId, this.convocatoriaId)
      .subscribe((mc) => {
        if (mc.configPerfilId === 2 || mc.configPerfilId === 3) {
          this.configuracionReqMinService
            .getGradosCarreras(this.perfilId, this.convocatoriaId)
            .subscribe((result) => {
              this.lstGradosFormacionACarrera = result;
            });
        }
      });
  }

  getFlagTipoBono() {
    this.evaluacionCurricularService
      .getFlagTipoBono(
        this.resumenTotal.resumenId,
        this.convocatoriaPostulanteId
      )
      .subscribe((res: any) => {
        if (res.tipoFFAA) this.tipoFFAA = res.tipoFFAA.split(',');

        if (res.tipoDeportista)
          this.tipoDeportista = res.tipoDeportista.split(',');

        if (res.tipoConadi) this.tipoConadi = res.tipoConadi.split(',');
      });
  }

  listarBonoConvocatoria(convocatoriaPostulante: number) {
    this.evaluacionCurricularService
      .listarBonoConvocatoria(convocatoriaPostulante)
      .subscribe((res: any) => {
        this.listarTipoBonificacion(res);
      });
  }

  listarTipoBonificacion(bonoConvocatoria: any) {
    this.maestraService
      .getMaestraDetalleByCod('TIP_BON')
      .subscribe((res: any) => {
        this.listarPorcentajeBono(bonoConvocatoria, res);
      });
  }

  listarPorcentajeBono(elements: any, tipo: any[]) {
    tipo.forEach((m) => {
      if (m.codProg === '1') {
        for (let t of elements.items) {
          if (m.maeDetalleId === t.tipoBonificacion) {
            this.lstBonoConadi = t.ltaDetalleBonificacion;
          }
        }
      } else if (m.codProg === '2') {
        for (let t of elements.items) {
          if (m.maeDetalleId === t.tipoBonificacion) {
            this.lstBonoDeportista = t.ltaDetalleBonificacion;
          }
        }
      } else if (m.codProg === '3') {
        for (let t of elements.items) {
          if (m.maeDetalleId === t.tipoBonificacion) {
            this.lstBonoFFAA = t.ltaDetalleBonificacion;
          }
        }
      }
    });
  }

  listarUrlDocumentos() {
    this.evaluacionCurricularService
      .listarUrlDocumentos(this.convocatoriaPostulanteId)
      .subscribe((res: any) => {
        this.urlConadi = res.urlConadis;
        this.urlDeportista = res.urlDeportista;
        this.urlFFAA = res.urlFFAA;
      });
  }

  listarResumen() {
    this.evaluacionCurricularService
      .evaluacionResumen(this.convocatoriaPostulanteId)
      .toPromise()
      .then((res: any) => {
        this.resumenTotal = res;
        this.getFlagTipoBono();
      })
      .catch((error: any) => { });
  }

  listarPuntajes() {
    this.evaluacionCurricularService
      .listarPuntajeEvaluacionCurricular(this.convocatoriaPostulanteId)
      .toPromise()
      .then((res: any) => {
        this.puntajes = res;
      });
  }

  listarFormacionAcademica() {
    this.evaluacionCurricularService
      .listarPostulanteFormacionAcademica(
        this.convocatoriaId,
        this.postulanteId,
        this.perfilId
      )
      .subscribe((res: any) => {
        if (res) {
          res.ltaSeccionFormacion.forEach((item: any) => { });
          this.listarFormacionByPerfil = res;
        }
      });

    this.evaluacionCurricularService
      .listarConvocatoriaCarreras(this.perfilId, this.convocatoriaId)
      .toPromise()
      .then((res: any) => {
        this.listarCarrerasPerfil = res;
      });

    this.evaluacionCurricularService
      .listarConvocatoriaEspecialidades(this.perfilId, this.convocatoriaId)
      .toPromise()
      .then((res: any) => {
        this.listarEspecialidadesPerfil = res;
      });
  }

  listarExperiencia() {
    this.listaExperienciaEntidad();
    this.evaluacionCurricularService
      .listarPostulanteExperiencia(
        this.convocatoriaId,
        this.postulanteId,
        this.perfilId
      )
      .toPromise()
      .then((res: any) => {
        this.listarExperienciaPostulante = res;
      });
  }

  listarInvestigacionPostulante() {
    const getInvestigacionConfig = this.evaluacionCurricularService.investigacionByPerfil(this.perfilId, this.convocatoriaId);
    const getInvestigacionPostulante =  this.evaluacionCurricularService.listarEvaluacionInvestigacion(this.convocatoriaPostulanteId);
    forkJoin([
      getInvestigacionConfig,
      getInvestigacionPostulante
    ]).subscribe((results) => {

      this.listaInvestigacionByPerfil = results[0];
      this.lstInvestigacionPostulante = results[1];

      this.lstInvestigacionPostulante.investigacion.listaInvestigacion.forEach((element , index) => {
        if (element.flagMarca !== null) {
          this.onCheckInvestigacion(element.flagMarca, index);
        }
      });

      this.lstInvestigacionPostulante.publicacion.listaInvestigacion.forEach((element , index) => {
        if (element.flagMarca !== null) {
          this.onCheckPublicacion(element.flagMarca, index);
        }
      });
    });

  }


  validarBonificacion(tipoBonificacion: number) {
    this.evaluacionCurricularService
      .setearBonificacion(this.resumenTotal.resumenId, tipoBonificacion)
      .subscribe((res: any) => {
        this.openModalBonificacion(res, tipoBonificacion);
      });
  }

  openModalBonificacion(res: any, tipo: number) {
    const dialogRef = this.dialog.open(ModalBonificacionConfirmComponent, {
      data: {
        tipoBonificacion: tipo,
        convocatoriaPostulante: this.convocatoriaPostulanteId,
        resumenId: this.resumenTotal.resumenId,
        patch: res,
      },
      width: '500px',
    });
    dialogRef.afterClosed().subscribe((any) => {
      if (any) {
        this.getFlagTipoBono();
      }
    });
  }

  toggleChange(event: any, item: any, tipo: string) {
    const dialogRef = this.dialog.open(ModalAlertConfirmComponent, {
      data: {
        type: 'input',
        checked: event.checked,
        item: item,
      },
    });

    dialogRef.afterClosed().subscribe((res: any) => {
      if (res && res.response) {
        let request: any = {
          trace: {
            traceId: 'string',
          },
          payload: {
            nombreSeccion: item.nombreSeccion,
            tipoSeccion: item.tipoSeccion,
            flagCalifica: item.flagCalifica === true ? 1 : 0,
            comentario: res.data,
          },
        };

        if (tipo === 'formacion') {
          request.payload['seccionFormacionId'] = item.seccionFormacionId;
          this.evaluacionCurricularService
            .actualizarSeccionEvaluacionFormac(item.seccionFormacionId, request)
            .toPromise()
            .then((rest: any) => {
              this.getData();
            });
        } else if (tipo === 'experiencia') {
          request.payload['seccionExperienciaId'] = item.seccionExperienciaId;
          this.evaluacionCurricularService
            .actualizarSeccionEvaluacionExp(item.seccionExperienciaId, request)
            .toPromise()
            .then((rest: any) => {
              this.getData();
            });
        } else if (tipo === 'investigacion' || tipo === 'publicacion') {
          this.evaluacionCurricularService.actualizarEvaluacionInvestigacion(this.lstInvestigacionPostulante).subscribe((mc) => {
            this.getData();
            this.toast.showToast("Calificación exitosa", 'info', 'Información');
          });
        }
      } else {
        item.flagCalifica = !item.flagCalifica;
      }
    });
  }

  confirmarFinalizado() {
    const dialogRef = this.dialog.open(ModalAlertConfirmComponent, {
      data: {
        type: 'alert',
        nombres: this.data.nombres,
      },
    });

    dialogRef.afterClosed().subscribe((result: any) => {
      if (result && result.response) {
        this.back();
      }
    });
  }

  verDocumentos(item: any) {
    const dialogRef = this.dialog.open(ModalVerDocumentoComponent, {
      data: {
        item: item,
      },
    });

    dialogRef.afterClosed().subscribe((result) => { });
  }

  validarLinkVisible(listaDocumentos: any[]) {
    if (!listaDocumentos) {
      return false;
    }

    let returned: number = 0;

    listaDocumentos.forEach((item: any) => {
      if (item.urlDocumento) {
        returned++;
      }
    });

    return returned > 0;
  }

  listaExperienciaEntidad() {
    this.evaluacionCurricularService
      .experiencialLaboralByPerfil(this.perfilId, this.convocatoriaId)
      .subscribe((res: any) => {
        this.listaExperienciaByPerfil = res;
      });
  }

  goForward(stepper: MatStepper) {
    this.selectedIndex = this.selectedIndex + 1;

    if (this.selectedIndex > 3) {
      this.selectedIndex = 3;
      this.confirmarFinalizado();
    } else {
      this.stepper.next();
    }
  }

  goBack(stepper: MatStepper) {
    this.selectedIndex = this.selectedIndex - 1;
    // window.scrollTo(0, 0);
    this.stepper.previous();
  }

  getExperienciaSeccionPostulante(tipoSeccion: number) {
    let seccion: any = null;

    this.listarExperienciaPostulante.forEach((item: any) => {
      if (tipoSeccion === item.tipoSeccion) {
        seccion = item;
      }
    });
    return seccion;
  }

  getSeccionesExperiencia(tipoSeccion: number) {
    return this.listarExperienciaPostulante.filter((item: any) => {
      return item.tipoSeccion !== tipoSeccion;
    });
  }

  getPerfilExperienciaBySeccion(seccion: number) {
    return this.listaExperienciaByPerfil.filter((item: any) => {
      return item.tipoExperiencia === seccion;
    });
  }

  listarOtrosRequisitosPostulante() {
    this.evaluacionCurricularService
      .listarOtrosRequisitosPostulante(this.convocatoriaPostulanteId)
      .toPromise()
      .then((res: any) => {
        this.listaOtrosRequisitoPostulante = res;
      });
  }

  ListaConfigOtrosrequisitos() {
    this.evaluacionCurricularService
      .ListaConfigOtrosrequisitos(this.perfilId)
      .toPromise()
      .then((res: any) => {
        this.listarConfiguracionOtrosReque = res;
      });
  }

  listarDeclaracionesJuradas() {
    this.configuracionReqMinService.getDeclaracionJuradaByPerfil(this.perfilId,this.convocatoriaId).subscribe((mc) => {
      this.lstDDJJEntidadServir = mc.filter((obj) => obj.isServir === '1');
      this.lstDDJJEntidadEspecifica = mc.filter((obj) => obj.isServir === '0');

    });
    this.evaluacionCurricularService
      .listarDeclaracionesJuradas(this.convocatoriaId, this.postulanteId)
      .toPromise()
      .then((res: any) => {
        this.indicadorPostulante = res.indicadorPostulante;
        this.ltaDDJJPostulanteServir = res.ltaDeclaracionNormal;
      });
  }

  getPuntajeSeccion(tipoSeccion: number, key: string) {
    let object = this.puntajes.find((item: any) => {
      return item.tipoSeccion === tipoSeccion;
    });

    if (object === null || object === undefined) {
      return '--';
    }

    if (object[key] === null || object[key] === undefined) {
      return '--';
    }

    return object[key];
  }

  getCalificaText(flag_apto: number) {
    if (flag_apto === null || flag_apto === undefined) {
      return '';
    }

    if (flag_apto === 1) {
      return 'CALIFICA';
    } else if (flag_apto === 2) {
      return 'NO CALIFICA';
    } else if (flag_apto === 3) {
      return 'DESCALIFICADO';
    } else {
      return 'NO CALIFICA';
    }
  }

  back() {
    this.router.navigateByUrl('pages/evaluacion-curricular/evaluacion-detalle');
  }

  verDocumento(urlDocumento: string) {
    window.open(Const.API_FILE_SERVER + urlDocumento, "_blank");
  }

  onCheckInvestigacion($event, i: number) {

    if (this.lstInvestigacionPostulante != null) {
      this.lstInvestigacionPostulante.investigacion.listaInvestigacion[i].flagMarca = $event;
    }

    this.cantMarcadosInvest = 0;
    this.lstInvestigacionPostulante.investigacion.listaInvestigacion.forEach(element => {
      if (element.flagMarca) {
        this.cantMarcadosInvest += 1;
      }

    });
    if (this.cantMarcadosInvest <= 0) {
      this.puntajeInvestigacion = 0;
    } else {
      let objEspecificInvest = this.listaInvestigacionByPerfil?.lstEspecifInvestigacion.find( (element) => {
        return element.cantidad === this.cantMarcadosInvest;

      });
      this.puntajeInvestigacion = objEspecificInvest.puntaje;
    }

    this.calcularPuntajesInvestigacion();

  }

  onCheckPublicacion($event, i: number) {
    if (this.lstInvestigacionPostulante != null) {
      this.lstInvestigacionPostulante.publicacion.listaInvestigacion[i].flagMarca = $event;
    }

    this.cantMarcadosPublic = 0;
    this.lstInvestigacionPostulante.publicacion.listaInvestigacion.forEach(element => {
      if (element.flagMarca) {
        this.cantMarcadosPublic += 1;
      }

    });
    if (this.cantMarcadosPublic <= 0) {
      this.puntajePublicacion = 0;
    } else {
      let objEspecificPublic = this.listaInvestigacionByPerfil?.lstEspecifPublicacion.find( (element) => {
        return element.cantidad === this.cantMarcadosPublic;

      });
      this.puntajePublicacion = objEspecificPublic.puntaje;
    }
    this.calcularPuntajesInvestigacion();
  }

  calcularPuntajesInvestigacion() {
    this.lstInvestigacionPostulante.puntajeParcial = (this.puntajePublicacion + this.puntajeInvestigacion);
    this.lstInvestigacionPostulante.puntajeTotal = (this.puntajePublicacion + this.puntajeInvestigacion) * (this.pesoELInvestigacion / 100);
  }
}
