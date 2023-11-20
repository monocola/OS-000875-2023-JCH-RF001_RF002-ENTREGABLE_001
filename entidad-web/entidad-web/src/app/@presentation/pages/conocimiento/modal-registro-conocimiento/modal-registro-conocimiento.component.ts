import { Component, Inject, Input, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { DetalleMaestra } from '../../../../@data/model/detalleMaestra';
import { MaestraRepository } from '../../../../@domain/repository/maestra.reposity';
import { ConocimientoRepository } from '../../../../@domain/repository/conocimiento.repository';
import { Conocimiento } from '../../../../@data/model/conocimiento';



@Component({
  selector: 'serv-talento-modal-registro-conocimiento',
  templateUrl: './modal-registro-conocimiento.component.html',
  styleUrls: ['./modal-registro-conocimiento.component.scss']
})

export class ModalRegistroConocimientoComponent implements OnInit {
  entidadId: any = 1;
  frm: FormGroup;
  lstTipoConocimiento: DetalleMaestra[];
  lstTipoCategoria: DetalleMaestra[];
  @Input() ismobileSize = false;
  isEdit: boolean = false;
  conocimiento: Conocimiento;

  constructor(
    private fb: FormBuilder,
    private toast: ToastService,
    protected ref: MatDialogRef<ModalRegistroConocimientoComponent>,
    private maestraService: MaestraRepository,
    private conocimientoService: ConocimientoRepository,
    @Inject(MAT_DIALOG_DATA) public data: Conocimiento
  ) {
    this.frm = this.fb.group({
      maeConocimientoId: new FormControl(null, ),
      tipoConocimientoId: new FormControl(null, Validators.required),
      categoriaConocimientoId: new FormControl(null, Validators.required),
      descripcion: new FormControl(null, Validators.required),
      estado: new FormControl(null, ),
      descripcionCategoria: new FormControl(null, ),
      descripcionTipo: new FormControl(null, ),
      codigoTipoConocimiento: new FormControl(null, ),
      entidadId: new FormControl(null, ),
    });

    if (this.data) {
      this.isEdit = true;
      this.setearConocimiento(this.data);
    }
  }

  ngOnInit(): void {
    this.initializeForm();
    this.listarCombox();
  }

  initializeForm() {

  }
  get f() {
    return this.frm.controls;
  }

  listarCombox(): void {
    this.getComboTipoConocimientos();
    this.getComboCategoriaConocimientos();
  }

  getComboTipoConocimientos() {
    this.maestraService
      .getMaestraDetalleByCod('TBL_MAE_TIPO_CONO')
      .subscribe((res) => {
        this.lstTipoConocimiento = res;
      });
  }

  getComboCategoriaConocimientos() {
    this.maestraService
      .getMaestraDetalleByCod('TBL_MAE_CATE_CONO')
      .subscribe((res) => {
        this.lstTipoCategoria = res;
      });
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  saveData() {
    this.conocimiento = null;
    if (this.frm.valid) {
        let body = this.getparse();
        this.conocimientoService.insertConocimiento(body)
          .subscribe((res) => {
            if ( res.length !== 0 ) {
              this.toast.showToast(
                'Se registro nuevo conocimiento.',
                'success',
                'Atención'
              );
            }
          });
        this.dismiss(true);
    }
  }

  getparse() {
    this.conocimiento = {
      maeConocimientoId: this.frm.value.maeConocimientoId,
      categoriaConocimientoId: this.frm.value.categoriaConocimientoId,
      descripcion: this.frm.value.descripcion,
      entidadId: null,
      codigoTipoConocimiento: null,
      descripcionTipo: null,
      descripcionCategoria: null,
      estado: "1",
      tipoConocimientoId: this.frm.value.tipoConocimientoId,
    };
    return this.conocimiento;
  }

  setearConocimiento(cono): void {
    this.conocimiento = cono;
    this.frm.patchValue(this.conocimiento);
  }

  editData() {
    if (this.frm.valid) {
      let request = this.mapearConocimiento();
      let maeConocimientoId = this.frm.value.maeConocimientoId;
      this.conocimientoService.updateConocimiento(request, maeConocimientoId)
        .subscribe((res) => {
          if ( res.length !== 0 ) {
            this.toast.showToast('Se ha actualizado con exito', 'success');
            this.dismiss(true);
          } else {
            this.toast.showToast(res.status.error.messages.toString(), 'danger');
          }
      });
      this.dismiss(true);
    }
  }

  mapearConocimiento(): any {
    let conocimiento: Conocimiento;
    conocimiento = this.frm.value;
    return conocimiento;
  }


}
