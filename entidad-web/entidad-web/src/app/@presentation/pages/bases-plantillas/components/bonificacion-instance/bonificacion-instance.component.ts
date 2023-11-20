import { Component, Input } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { Bonificacion } from '../../creacion-form-base/bonificacion/bonificacion.component';
import { CreacionFormBaseService } from '../../creacion-form-base/creacion-form-base.service';
import { ModalCreacionBonificacionComponent } from '../modal-creacion-bonificacion/modal-creacion-bonificacion.component';

@Component({
  selector: 'serv-talento-bonificacion-instance',
  templateUrl: './bonificacion-instance.component.html',
  styleUrls: ['./bonificacion-instance.component.scss'],
})
export class BonificacionInstanceComponent {
  @Input() form: FormGroup;
  @Input() bonificaciones: Bonificacion[] = [];

  textSize = 0;
  editorStyle = {
    height: '100px',
  };
  constructor(
    private dialog: MatDialog,
    private helperService: CreacionFormBaseService
  ) {}

  get f() {
    return this.form.controls;
  }

  getValueLength(e) {
    this.textSize = e.text.length;
  }

  returnTiposBonificacion() {
    let bonsSelected: number[] = this.bonificaciones.map(
      (b) => b.form.value.tipoBonificacion?.maeDetalleId || null
    );
    if (this.f.tipoBonificacion.value) {
      const index = bonsSelected.indexOf(
        this.f.tipoBonificacion.value.maeDetalleId
      );
      bonsSelected.splice(index, 1);
      return this.helperService.tipoBonificaciones.filter(
        (b) => !bonsSelected.includes(b.maeDetalleId)
      );
    } else {
      return this.helperService.tipoBonificaciones.filter(
        (b) => !bonsSelected.includes(b.maeDetalleId)
      );
    }
  }

  addBonificacion(bonificacion = null) {
    const modalBonificacion = this.dialog.open(
      ModalCreacionBonificacionComponent,
      {
        width: '45rem',
        data: {
          form: bonificacion,
          detalles: this.f.bonificaciones.value,
        },
      }
    );
    modalBonificacion.afterClosed().subscribe((res) => {
      this.f.bonificaciones.markAsTouched();
      if (res) {
        this.f.bonificaciones.patchValue([
          ...this.f.bonificaciones.value,
          {
            descripcion: res.descripcion,
            niveles: res.niveles,
            aplicaSobre: res.aplicaSobre || '',
            porcentaje: res.porcentaje,
            id: res.id,
          },
        ]);
      }
    });
  }

  editBonificacion(bonificacion, index) {
    const modalBonificacion = this.dialog.open(
      ModalCreacionBonificacionComponent,
      {
        width: '45rem',
        data: {
          form: bonificacion,
          detalles: this.f.bonificaciones.value,
        },
      }
    );
    modalBonificacion.afterClosed().subscribe((res) => {
      this.f.bonificaciones.markAsTouched();
      if (res) {
        const bonificaciones = [...this.f.bonificaciones.value];
        bonificaciones[index] = { ...res };
        this.f.bonificaciones.patchValue([...bonificaciones]);
      }
    });
  }

  deleteDetailBonificacion(index) {
    const deleteModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Limpiar bonificación',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    deleteModal.afterClosed().subscribe((res) => {
      this.f.bonificaciones.markAsTouched();
      if (res) {
        const bonificaciones = [...this.f.bonificaciones.value];
        if (bonificaciones[index].id) {
          const bonificacionesToDelete = [
            ...this.f.bonificacionesToDelete.value,
          ];
          this.f.bonificacionesToDelete.patchValue([
            ...bonificacionesToDelete,
            bonificaciones[index],
          ]);
        }
        bonificaciones.splice(index, 1);
        this.f.bonificaciones.patchValue([...bonificaciones]);
      }
    });
  }
}
