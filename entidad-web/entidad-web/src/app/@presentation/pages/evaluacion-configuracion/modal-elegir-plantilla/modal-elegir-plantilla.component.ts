import { ToastService } from './../../../@common-components/toast';
import { FormGroup, FormBuilder, FormControl, Validators } from '@angular/forms';
import { Router } from '@angular/router';
import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';
import { Component, OnInit } from '@angular/core';
import { MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'serv-talento-modal-elegir-plantilla',
  templateUrl: './modal-elegir-plantilla.component.html',
  styleUrls: ['./modal-elegir-plantilla.component.scss']
})
export class ModalElegirPlantillaComponent implements OnInit {

  frmElegirPlantilla: FormGroup;
  lstPlantillas: any;

  constructor(
    private matDialogRef: MatDialogRef<ModalElegirPlantillaComponent>,
    private fb: FormBuilder,
    private configuracionReqMinService: ConfiguracionReqMinRepository,
    private router: Router,
    private toast: ToastService,
  ) {
    this.frmElegirPlantilla = this.fb.group({
      selPlantilla: new FormControl(null, Validators.required),
    });
  }

  ngOnInit(): void {
    this.cargarPlantillas();
  }

  cargarPlantillas() {
    this.configuracionReqMinService.getPlantillasConfiguracion().subscribe((mc) => {
     this.lstPlantillas = mc;
    });
  }

  dismiss(result: boolean): void {
    this.matDialogRef.close(true);
  }

  aceptar() {
    let frm = this.frmElegirPlantilla.getRawValue();
    if (frm.selPlantilla !== null && frm.selPlantilla !== "") {
      localStorage.setItem("selectedPlantilla",frm.selPlantilla);
      this.router.navigateByUrl("pages/configuracion-evaluacion/configuracion-requisitos-minimos");
      this.dismiss(true);
    } else {
      this.toast.showToast('Elija su plantilla de configuración', 'warning', 'Plantilla no seleccionada');
    }

  }

}
