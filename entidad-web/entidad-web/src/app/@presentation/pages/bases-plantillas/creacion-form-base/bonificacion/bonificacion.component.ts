import { Location } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { Router } from '@angular/router';
import { BasesPlantillasRepository } from 'src/app/@domain/repository/bases-plantillas.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { CreacionFormBaseService } from '../creacion-form-base.service';

@Component({
  selector: 'serv-talento-bonificacion',
  templateUrl: './bonificacion.component.html',
  styleUrls: ['./bonificacion.component.scss'],
})
export class BonificacionComponent implements OnInit {
  bodySize = '62.5rem';

  bonificaciones: Bonificacion[] = [];
  bonificacionesToDelete: Bonificacion[] = [];
  tituloGeneral = new FormControl('', Validators.required);

  constructor(
    private location: Location,
    private fb: FormBuilder,
    private basesService: BasesPlantillasRepository,
    private helperService: CreacionFormBaseService,
    private toastService: ToastService,
    public router: Router
  ) {}

  ngOnInit(): void {
    this.helperService.tipoInforme = JSON.parse(
      sessionStorage.getItem('tipoInforme')
    );
    if (this.helperService.dataToEdit) {
      this.setDataBonificaciones(this.helperService.dataToEdit);
      this.helperService.informeIdToEdit = this.helperService.dataToEdit.informeDetalleId;
    }
  }

  setDataBonificaciones(values) {
    this.basesService
      .getBonificacionList(values.informeDetalleId)
      .subscribe((results: any[]) => {
        this.tituloGeneral.patchValue(values.titulo);
        results.forEach((el) => {
          this.bonificaciones.push({
            open: true,
            bonificacionId: el.bonificacionId,
            form: this.fb.group({
              titulo: [el.titulo, Validators.required],
              contenido: [el.contenido, Validators.required],
              tipoBonificacion: [
                this.helperService.tipoBonificaciones.find(
                  (a) => a.maeDetalleId === el.tipoBonificacion
                ),
                [Validators.required],
              ],
              bonificaciones: this.setDetalleBonificaciones(
                el.bonificacionDetalleDTOList
              ),
              bonificacionesToDelete: [[]],
            }),
          });
        });
      });
  }

  setDetalleBonificaciones(DTOdetalle: any[]) {
    const detalles = DTOdetalle.map((d) => {
      return {
        descripcion: d.descripcion,
        niveles: this.helperService.nivelesBonificaciones.find(
          (nivel) => nivel.maeDetalleId === d.nivelId
        ),
        aplicaSobre: this.helperService.aplicaSobre.find(
          (aplica) => aplica.maeDetalleId === d.aplicaId
        ),
        porcentaje: d.porcentajeBono,
        id: d.bonificacionDetalleId,
      };
    });
    return [[...detalles]];
  }

  addBonificacion() {
    this.bonificaciones.push({
      open: true,
      bonificacionId: null,
      form: this.fb.group({
        titulo: ['', Validators.required],
        contenido: ['', Validators.required],
        tipoBonificacion: ['', Validators.required],
        bonificaciones: [[]],
        bonificacionesToDelete: [[]],
      }),
    });
  }

  deleteBonificacion(index) {
    if (this.bonificaciones[index].bonificacionId) {
      this.bonificacionesToDelete = [
        ...this.bonificacionesToDelete,
        this.bonificaciones[index],
      ];
    }
    this.bonificaciones.splice(index, 1);
  }

  hide(index) {}

  onResized(e) {
    this.bodySize = e.newWidth + 'px';
  }

  goBack() {
    this.location.back();
  }

  save() {
    let invalidForm = false;
    this.bonificaciones.forEach((b) => {
      b.form.markAllAsTouched();
      if (
        b.form.invalid ||
        b.form.get('bonificaciones').value.length === 0 ||
        this.tituloGeneral.invalid
      ) {
        invalidForm = true;
      }
    });
    this.tituloGeneral.markAsTouched();
    if (invalidForm) return;
    const body = {
      tipoInformeId: this.helperService.tipoInforme.maeDetalleId,
      tipoInforme: this.helperService.tipoInforme.descripcion,
      nombreInforme: this.tituloGeneral.value,
      contenido: null,
      bonificaciones: this.bonificaciones
        .map((b) => {
          return {
            id: b.bonificacionId,
            informeDetalleId: this.helperService.informeIdToEdit,
            estado: 1,
            form: b.form.getRawValue(),
          };
        })
        .concat(
          this.bonificacionesToDelete.map((b) => {
            return {
              id: b.bonificacionId,
              informeDetalleId: this.helperService.informeIdToEdit,
              estado: 0,
              form: b.form.getRawValue(),
            };
          })
        ),
    };
    this.basesService
      .saveOrUpdatePlantillaBase(body, this.helperService.informeIdToEdit)
      .subscribe((res) => {
        this.helperService.informeIdToEdit
          ? this.toastService.showToast(
              'Se guardaron los cambios exitosamente',
              'primary'
            )
          : this.toastService.showToast(
              'El informe ha sido creado exitosamente',
              'success'
            );
        this.helperService.initializeValues();
        this.router.navigateByUrl('pages/gestionplantillas');
      });
  }
}

export interface Bonificacion {
  open: boolean;
  bonificacionId: number;
  form: FormGroup;
}
