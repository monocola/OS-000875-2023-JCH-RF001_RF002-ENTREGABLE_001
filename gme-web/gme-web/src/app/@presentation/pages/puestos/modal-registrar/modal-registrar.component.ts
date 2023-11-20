import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { PuestoRepository } from 'src/app/@domain/repository/puesto.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-modal-registrar',
  templateUrl: './modal-registrar.component.html',
  styleUrls: ['./modal-registrar.component.scss']
})
export class ModalRegistrarComponent implements OnInit {
  editionMode: boolean = false;
  form: FormGroup;
  mensaje: any;
  sigla: any;
  codigo: any;
  selectOrganigrama;

  unidadOrganicaCbo: UnidadOrganicaCombo[] = [];
  searchMode = false;
  constructor(
    private matDialog: MatDialogRef<ModalRegistrarComponent>,
    private fb: FormBuilder,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private puestoRepository: PuestoRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private toastService: ToastService,
  ) { }

  ngOnInit(): void {
    this.loadCombox();
    this.initializeForm();
  }

  get f() {
    return this.form.controls;
  }
  initializeForm() {
    if (this.data.createMode) {
      this.form = this.fb.group({
        descripcion: ['', [Validators.required, Validators.maxLength(500)]],
        organigramaId: ['', [Validators.required]],
        esJefe: ['N'],
      });
    } else {
      const uo = this.data.dataToEdit;
      this.selectOrganigrama = uo.uoId;

      this.form = this.fb.group({
        descripcion: [ uo.nombrePuesto, [ Validators.required, Validators.maxLength(300)] ],
        organigramaId: [ Number(uo.uoId), [Validators.required] ],
        esJefe: 'N',
      });
      if (uo.esJefe === "SI") {
        this.form.patchValue({
          esJefe: 'S'
        });
      }
    }
  }

  loadCombox() {
    this.searchMode = false;
    const getUO = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    forkJoin([getUO])
      .subscribe(
        (results) => {
          this.unidadOrganicaCbo = results[0];
        },
        (error) => { console.log(error); }
      );
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }


  save() {
    this.form.markAllAsTouched();
    if (this.form.valid) {
      const puestoExist =  {
        unidadOrganicaID: this.form.get('organigramaId').value,
        nombrePuesto: '',
        esJefe: '',
        puestoId: ''
      };
      this.puestoRepository.searchPuestos(puestoExist).subscribe(
        (results) => {
          let var23 = results.find(pues => pues.nombrePuesto === this.form.get('descripcion').value.toUpperCase() && pues.puestoId !== this.data?.dataToEdit?.puestoId);
          let var24 = results.filter(pues => pues.esJefe === 'SI' && pues.puestoId !== this.data?.dataToEdit?.puestoId);
          if (var23 ) {
            this.toastService.showToast(
              'Ya existe un puesto con el mismo nombre',
              'danger'
            );
          } else {
            if (var24.length > 0 && this.form.get('esJefe').value == 'S') {
              this.toastService.showToast(
                'Esta opción ya se encuentra asignada',
                'danger'
              );
            } else {           
              this.registerUpdate();
            }
            
          }
        }
      );
    }
  }

  registerUpdate() {
    const body = this.form.getRawValue();
    this.puestoRepository.registerOrUpdate( body, this.data?.dataToEdit?.puestoId || null )
        .subscribe((res) => {
          if (res) {
            this.toastService.showToast(this.data.createMode ?
              'Se realizó el registro exitosamente' :
              'Se realizó la actualización exitosamente',
              'success'
            );
            this.onNoClick(true);
          }
        }, (err) => this.toastService.showToast(err, 'danger')
        );
  }
}

export interface DataModel {
  createMode: boolean;
  dataToEdit: any;
  estados: any[];
}

