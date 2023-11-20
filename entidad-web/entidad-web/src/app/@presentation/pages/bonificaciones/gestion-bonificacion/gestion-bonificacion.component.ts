import { Location } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { BonificacionesHelperService } from '../bonificaciones-helper.service';
import { ModalNuevoDetalleBonificacionComponent } from '../modal-nuevo-detalle-bonificacion/modal-nuevo-detalle-bonificacion.component';
import { BonificacionesRepository } from '../../../../@domain/repository/bonificaciones.repository';
import { ToastService } from '../../../@common-components/toast';
import { BonificacionMensajesUser } from '../bonificaciones-constantes';
import { Bonificacion } from '../../../../@data/model/bonificaciones/bonificacion';

@Component({
  selector: 'serv-talento-gestion-bonificacion',
  templateUrl: './gestion-bonificacion.component.html',
  styleUrls: ['./gestion-bonificacion.component.scss'],
})
export class GestionBonificacionComponent implements OnInit {
  bodySize = '62.5rem';
  bonificacionForm: FormGroup;
  niveles: any[] = [];
  textSize = 0;
  textSizeMax = 5000;
  editorStyle = {
    height: '125px',
  };
  private bonificacionId: number = null;
  private bonificacion: Bonificacion;

  constructor(
    public helperService: BonificacionesHelperService,
    private bonificacionRepository: BonificacionesRepository,
    private router: Router,
    private route: ActivatedRoute,
    private location: Location,
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toast: ToastService,
  ) {
    this.route.params.subscribe(params => {
      this.bonificacionId = params.bonificacionId;
    });
    this.initializeForms();
  }

  ngOnInit(): void {
    this.helperService.loadCombox();
    if (this.bonificacionId != null) {
      setTimeout(() => {
        this.fluejoEdit();
      }, 200);
    } else {
      if (sessionStorage.getItem('tipoBonificacion')) {
        this.helperService.bonificacionSelected = JSON.parse(
          sessionStorage.getItem('tipoBonificacion')
        );
      } else {
        this.router.navigateByUrl('pages/gestionbonificaciones');
      }
    }

  }

  private fluejoEdit() {
    this.bonificacionRepository.detail(this.bonificacionId)
      .subscribe(result => {
        this.bonificacion = result;
        console.info(this.bonificacion);
        this.setDetail();
      }, error => {
        console.info(error);
        this.toast.showToast(BonificacionMensajesUser.BON_DETALLE_GET_ERROR, 'danger');
        this.router.navigateByUrl('pages/gestionbonificaciones');
      });
  }

  addNivel(bonificacion = null) {
    const modalNivel = this.dialog.open(
      ModalNuevoDetalleBonificacionComponent,
      {
        width: '45rem',
        data: {
          form: bonificacion,
          detalles: this.f.niveles.value,
        },
      }
    );
    modalNivel.afterClosed().subscribe((res) => {
      this.f.niveles.markAsTouched();
      if (res) {
        this.f.niveles.patchValue([
          ...this.f.niveles.value,
          {
            niveles: res.niveles,
            aplicaSobre: res.aplicaSobre || '',
            porcentaje: res.porcentaje,
            id: res.id,
            descripcion: res.descripcion,
          },
        ]);
      }
    });
  }

  setDetail() {
    this.f.titulo.patchValue(this.bonificacion.titulo);
    this.f.resumen.patchValue(this.bonificacion.contenido);
    this.f.niveles.patchValue(this.bonificacion.bonificacionDetalleDTOList
      .map(item => {
        return {
          descripcion: item.descripcion,
          niveles: this.helperService.niveles.find(a => a.maeDetalleId === item.nivelId),
          aplicaSobre: this.helperService.aplicaSobre.find((a) => a.maeDetalleId === item.aplicaId),
          porcentaje: item.porcentajeBono,
          id: item.bonificacionDetalleId,
        };
      }));
  }

  initializeForms() {
    this.bonificacionForm = this.fb.group({
      titulo: ['', Validators.required],
      resumen: ['', Validators.required],
      niveles: [[], Validators.required],
    });
  }

  private getDataSave() {
    return {
      bonificacionId: this.bonificacionId,
      tipoInfoId: null,
      tipoBonificacion: this.helperService.bonificacionSelected.maeDetalleId,
      titulo: this.f.titulo.value,
      contenido: this.f.resumen.value,
      estadoRegistro: '1',
      bonificacionDetalleDTOList: this.f.niveles.value.map(item => {
        return {
          bonificacionDetalleId: item.id,
          bonificacionId: this.bonificacionId,
          descripcion: item.descripcion,
          nivelId: item.niveles.maeDetalleId,
          aplicaId: item.aplicaSobre.maeDetalleId,
          porcentajeBono: item.porcentaje,
          nombreNivel: null,
          nombreAplica: null,
          estadoRegistro: '1',
        };
      })
    };
  }

  private getDataUpdate() {
    let data = {
      bonificacionId: this.bonificacionId,
      tipoInfoId: this.bonificacion.tipoInfoId,
      tipoBonificacion: this.bonificacion.tipoBonificacion,
      titulo: this.f.titulo.value,
      contenido: this.f.resumen.value,
      estadoRegistro: '1',
      bonificacionDetalleDTOList: this.bonificacion.bonificacionDetalleDTOList.map(item => {
        let dataBoni = this.f.niveles.value.find(value => value.id === item.bonificacionDetalleId);
        return {
          bonificacionDetalleId: dataBoni == null ? item.bonificacionDetalleId : dataBoni.id,
          bonificacionId: this.bonificacionId,
          descripcion: item.descripcion,
          nivelId: dataBoni == null ? item.nivelId : dataBoni.niveles.maeDetalleId,
          aplicaId: dataBoni == null ? item.aplicaId : dataBoni.aplicaSobre.maeDetalleId,
          porcentajeBono: dataBoni == null ? item.porcentajeBono : dataBoni.porcentaje,
          nombreNivel: null,
          nombreAplica: null,
          estadoRegistro: dataBoni == null ? '0' : '1',
        };
      })
    };

    data.bonificacionDetalleDTOList = data.bonificacionDetalleDTOList.concat(this.f.niveles.value
      .filter(item => data.bonificacionDetalleDTOList.map(itemold => itemold.bonificacionDetalleId).indexOf(item.id) === -1)
      .map(item => {
        return {
          bonificacionDetalleId: item.id,
          bonificacionId: this.bonificacionId,
          descripcion: item.descripcion,
          nivelId: item.niveles.maeDetalleId,
          aplicaId: item.aplicaSobre.maeDetalleId,
          porcentajeBono: item.porcentaje,
          nombreNivel: null,
          nombreAplica: null,
          estadoRegistro: '1',
        };
      }));
    return data;
  }

  saveNivel() {
    if (this.bonificacionId == null) {
      this.createBonificacion();
    } else {
      this.updateBonificacion();
    }
  }

  createBonificacion() {
    if (this.textSize > this.textSizeMax) {
      this.toast.showToast("El contenido de la bonificación es mayor al permitido", 'danger');
    } else {
      this.bonificacionRepository.saveBonificacion(this.getDataSave())
        .subscribe(result => {
          if (result) {
            this.toast.showToast(BonificacionMensajesUser.BON_CREAR_OK, 'primary');
            this.router.navigateByUrl('/pages/gestionbonificaciones');
          } else {
            this.toast.showToast(BonificacionMensajesUser.BON_CREAR_ERROR, 'danger');
          }
        }, error => this.toast.showToast(error.message, 'danger'));
    }
  }

  updateBonificacion() {
    if (this.textSize > this.textSizeMax) {
      this.toast.showToast("El contenido de la bonificación es mayor al permitido", 'danger');
    } else {
      this.bonificacionRepository.updateBonificacion(this.getDataUpdate())
        .subscribe(result => {
          if (result) {
            this.toast.showToast(BonificacionMensajesUser.BON_UPDATE_OK, 'primary');
            this.router.navigateByUrl('/pages/gestionbonificaciones');
          } else {
            this.toast.showToast(BonificacionMensajesUser.BON_UPDATE_ERROR, 'danger');
          }
        }, error => this.toast.showToast(error.message, 'danger'));
    }
  }

  editDetalle(detalle, index) {
    const modalDetalle = this.dialog.open(
      ModalNuevoDetalleBonificacionComponent,
      {
        width: '45rem',
        data: {
          form: detalle,
          detalles: this.f.niveles.value,
        },
      }
    );
    modalDetalle.afterClosed().subscribe((res) => {
      this.f.niveles.markAsTouched();
      if (res) {
        const detalles = [...this.f.niveles.value];
        detalles[index] = { ...res };
        this.f.niveles.patchValue([...detalles]);
      }
    });
  }

  deleteDetailBonificacion(index) {
    this.f.niveles.patchValue(this.f.niveles.value.filter(((item, indexOrigin) => indexOrigin !== index)));
  }

  get f() {
    return this.bonificacionForm.controls;
  }

  getValueLength(e) {
    this.textSize = e.text.length;
  }

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  goBack() {
    this.location.back();
  }

}
