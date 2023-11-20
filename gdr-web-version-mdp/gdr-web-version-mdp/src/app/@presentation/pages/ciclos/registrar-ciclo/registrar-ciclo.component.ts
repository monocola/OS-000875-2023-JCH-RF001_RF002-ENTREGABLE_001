import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { DatePipe } from '@angular/common';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { CicloService } from '../../../../@data/services/ciclo.service';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { Puesto } from '../../../../@data/model/puesto';
import { ToastService } from '../../../@common-components/toast';
import { Utils } from 'src/app/utils/utils';

@Component({
  selector: 'serv-talento-registrar-ciclo',
  templateUrl: './registrar-ciclo.component.html',
  styleUrls: ['./registrar-ciclo.component.scss']
})
export class RegistrarCicloComponent implements OnInit {
  constructor(
    private fb: FormBuilder,
    protected ref: MatDialogRef<RegistrarCicloComponent>,
    private authenticationService: AuthenticationRepository,
    private datePipe: DatePipe,
    private maeParametroRepository: MaestraParametroRepository,
    private cicloService: CicloService,
    private toast: ToastService,
  ) {
    this.frm = this.fb.group({
      anio: new FormControl(null,
        [Validators.required, Validators.minLength(4), Validators.maxLength(4)]
      ),
      fechaIni: new FormControl(null, Validators.required),
      fechaFin: new FormControl(null, Validators.required),
  /*     puestoTitular: new FormControl(null, Validators.required),
      puestoJefe: new FormControl(null, Validators.required),
      puestoGestor: new FormControl(null, Validators.required), */
    });
  }

  frm: FormGroup;
  minDate;
  maxDate;
  profile = this.authenticationService.getCurrentUserValue;
  estados: MaestraParametro[];
  puestoTitular: Puesto[];
  puestoJefe: Puesto[];
  puestoGestor: Puesto[];
  estadoIdSeleccionado;
  estadoDescSeleccionado;
  ngOnInit(): void {
    this.loadCombox();
    console.info(this.profile);

    setTimeout(() => {
      const fechaIni = document.getElementById('fechaIni');

      fechaIni.setAttribute('maxlength', '10');

      fechaIni.onkeydown = (e: any) => {
        return this.isNumeric(fechaIni, e.keyCode);
      };

      fechaIni.onkeyup = (e: any) => {
        this.validateDateFormat(fechaIni, e.keyCode);
      };

      const input = document.getElementById('fechaFin');

      input.setAttribute('maxlength', '10');

      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };

      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };
    }, 250);
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 && keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }

  loadCombox() {
    const getEstados = this.maeParametroRepository.getMaestraParametro('CICLO_ESTADO');
    const getPuesto = this.cicloService.getPuesto(this.profile.entidadId, null);
    forkJoin([getEstados, getPuesto]).subscribe(
      (results) => {
        console.log(results[1]);
        this.estados = results[0];
        this.puestoTitular = results[0];
        this.puestoJefe = results[1];
        this.puestoGestor = results[1];
      },
    );

  }

  get f() {
    return this.frm.controls;
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  validaCampos() {
    let isValid = true;

    if (this.frm.value.anio == null ) {
      isValid = false;
      this.toast.showToast('El campo anio debe ser mayor a 0', 'danger');
    }

    // validar fechas
    let fecIni = Utils.formatFechaDate(
      this.frm.value.fechaIni,
      'DD/MM/YYYY'
    );
    let fecFin = Utils.formatFechaDate(
      this.frm.value.fechaFin,
      'DD/MM/YYYY'
    );

    if (fecIni >= fecFin) {
      this.toast.showToast(
        'La fecha de inicio debe ser menor a la fecha de fin ',
        'danger'
      );
      this.f.fechaInicio.setValue(null);
      this.f.fechaFin.setValue(null);
      isValid = false;
    }
    return isValid;
  }

  addCiclo() {
   // const estadoRegistrar = this.buscaEstado(1);
    let isValid = this.validaCampos();
    if ( isValid ) {
      if ( this.frm.valid ) {
        this.cicloService.registraCiclo(
          this.profile.entidadId,
      /*     this.frm.value.puestoTitular.id,
          this.frm.value.puestoJefe.id,
          this.frm.value.puestoGestor.id, */
          this.frm.value.anio,
          this.frm.value.fechaIni,
          this.frm.value.fechaFin,
          null,
          /* estadoRegistrar.codigoNumero,
          estadoRegistrar.valorTexto */
        ).subscribe( (res) => {
          if ( res.status.success) {
            this.toast.showToast('Se registro el ciclo con Exito', 'success', 'Atención');
            this.dismiss(res.payload);
          } else {
            this.toast.showToast(res.estatus.error.messages[0], 'danger', 'Atención');
          }
        });
      } else {
        this.toast.showToast('Debe completar los campos obligatorios', 'danger');
      }
    }
  }

/*   buscaEstado(codigoNumero: number) {
    if ( this.estados ) {
      return this.estados.find( item => item.codigoNumero === codigoNumero);
    }
  } */
}
