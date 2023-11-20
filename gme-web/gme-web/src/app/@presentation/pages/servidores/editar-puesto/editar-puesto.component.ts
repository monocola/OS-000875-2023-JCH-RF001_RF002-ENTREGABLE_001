import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { PuestoUoServidorCivil } from 'src/app/@data/model/puesto';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-editar-puesto',
  templateUrl: './editar-puesto.component.html',
  styleUrls: ['./editar-puesto.component.scss'],
})
export class EditarPuestoComponent implements OnInit {
  registerForm: FormGroup;
  unidadesOrganicas: UnidadOrganicaCombo[] = [];
  tiposAsignacion: MaestraParametro[] = [];

  listaEncargados: any[] = [];
  listMotivosEncarturas: any[] = [];
  listaPuestos: any[] = [];
  // ultimoPuestoId: number = 0;
  TIPO_ASIGNACION_ENCARGADO: number = 2;

  constructor(
    private fb: FormBuilder,
    private toastService: ToastService,
    private matDialog: MatDialogRef<EditarPuestoComponent>,
    private servidoresRepository: ServidoresRepository,
    private maeParametroRepository: MaestraParametroRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    @Inject(MAT_DIALOG_DATA) public data: PuestoUoServidorCivil
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.cargarCombos();
  }

  get f() {
    return this.registerForm.controls;
  }

  cargarCombos() {
    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    const getTipoAsignacion = this.maeParametroRepository.getMaestraParametro(
      'TIPO_ASIGNACION_SERVIDOR_CIVIL'
    );
    const uoId = this.unidadesOrganicas.find(
      (x) => x.sigla === this.data.siglaUO
    )?.id;
    console.info(uoId, 'ID');
    const getPuestos = this.unidadOrganicaRepository.getPuestos({
      unidadOrganicaID: uoId,
      nombrePuesto: '',
      esJefe: '',
      puestoId: '',
    });
    console.info(getPuestos);
    const getMotivoEncargadura = this.maeParametroRepository.getMaestraParametro(
      'MOTIVO_ENCARGATURA'
    );

    forkJoin([
      getUndOrganicaCbo,
      getTipoAsignacion,
      getPuestos,
      getMotivoEncargadura,
    ]).subscribe(
      (results) => {
        this.unidadesOrganicas = results[0];
        this.tiposAsignacion = results[1];
        this.listaPuestos = results[2];
        console.info(this.listaPuestos);
        this.listMotivosEncarturas = results[3];
        setTimeout(() => this.cargarDatos());
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      organo: ['', [Validators.required]],
      puesto: ['', [Validators.required]],
      tipoAsignacion: [{ value: '', disabled: true }, [Validators.required]],
      motivoId: ['', [Validators.required]],
      fechaInicio: ['', [Validators.required]],
      fechaCese: [''],
      // accionPuesto: [{ value: '', disabled: true }],
    });

    setTimeout(() => {
      const input = document.getElementById('fechaInicioNuevo');
      input.setAttribute('maxlength', '10');

      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };

      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };

      const fechaCese = document.getElementById('fechaCese');

      fechaCese.setAttribute('maxlength', '10');

      fechaCese.onkeydown = (e: any) => {
        return this.isNumeric(fechaCese, e.keyCode);
      };

      fechaCese.onkeyup = (e: any) => {
        this.validateDateFormat(fechaCese, e.keyCode);
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
        keyCode !== 8 &&
        keyCode !== 46
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
    //let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    //if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    //} else {
      // Es invalido
    //}
  }

  cargarDatos() {
    const uoId = this.unidadesOrganicas.find(
      (x) => x.sigla === this.data.siglaUO
    )?.id;

    this.listaPuestos = this.listaPuestos.filter(
      (item) => item.uoId === uoId.toString()
    );

    this.registerForm.get('organo').patchValue(uoId);
    this.registerForm.get('motivoId').patchValue(Number(this.data.motivoId));
    this.registerForm.get('puesto').patchValue(this.data.puestoId.toString());
    this.registerForm
      .get('tipoAsignacion')
      .patchValue(Number(this.data.tipoAsignacion));
    // this.registerForm.get('personaAsignada').setValue(this.data.personaAsignada);
    this.registerForm
      .get('fechaInicio')
      .setValue(new Date(this.data.fechaInicio));
    if (this.data.fechaCese) {
      this.registerForm
        .get('fechaCese')
        .setValue(new Date(this.data.fechaCese));
    } else {
      this.registerForm.get('fechaCese').setValue(null);
    }
  }

  /*onSelectionChangePuesto(puestoId: number) {
    let seleccionado = this.listaPuestos.find(x => x.id === puestoId);
    if (seleccionado) {
      this.ultimoPuestoId = puestoId;
      //this.registerForm.get('accionPuesto').reset();
      //this.registerForm.get('accionPuesto').disable();
      this.registerForm.get('puesto').setValue(seleccionado.descripcion);
    }
  }*/

  actualizar() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      const uoId: number = this.registerForm.get('organo').value;
      // const accionPuesto = this.registerForm.get('accionPuesto').value;
      const personaAsignada = this.data.personaId;

      const puesto = this.registerForm.get('puesto').value;

      const datos: PuestoUoServidorCivil = {
        detuoId: this.data?.detalleuoId,
        uoId: uoId,
        personaId: this.data.personaId,
        puestoId: puesto,
        motivoId: this.registerForm.get('motivoId').value,
        tipoAsignacion: this.registerForm.get('tipoAsignacion').value,
        personaIdAsignada: personaAsignada ? personaAsignada : null,
        fechaInicio: this.registerForm.get('fechaInicio').value,
        fechaCese: this.registerForm.get('fechaCese').value,
      };

      this.servidoresRepository.editarPuesto(datos).subscribe(
        (x) => {
          this.cerrar(true);
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
          console.log(err);
        }
      );
    }
  }

  actualizarEncargados() {
    const idUO = this.registerForm.get('organo').value;
    const tipoAsignacion = this.registerForm.get('tipoAsignacion').value;
    if (idUO && tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      this.servidoresRepository
        .listarPersonasParaPuesto(idUO, tipoAsignacion)
        .subscribe((x) => {
          this.listaEncargados = x;
        });
    } else {
      this.listaEncargados = [];
    }
  }

  onSelectionChangeTipoAsignacion(tipoAsignacion: number) {
    this.actualizarEncargados();
    const control = this.registerForm.get('personaAsignada');
    if (tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      control.setValidators(Validators.required);
    } else {
      control.clearValidators();
    }
    control.updateValueAndValidity();
  }

  onSelectionChangeUO(uoId?: number) {
    this.registerForm.get('puesto').setValue("");
    this.actualizarEncargados();
    this.unidadOrganicaRepository
      .getPuestos({
        unidadOrganicaID: uoId,
        nombrePuesto: '',
        esJefe: '',
        puestoId: '',
      })
      .subscribe((res) => (this.listaPuestos = res));
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }
}
