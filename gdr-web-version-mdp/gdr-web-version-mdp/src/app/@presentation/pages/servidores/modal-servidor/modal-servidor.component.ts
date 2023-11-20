import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { DatosPersonalesServidorCivil } from 'src/app/@data/model/servidores-civiles';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { UnidadOrganicaRepository } from '../../../../@domain/repository/unidad-organica.repository';

@Component({
  selector: 'serv-talento-modal-servidor',
  templateUrl: './modal-servidor.component.html',
  styleUrls: ['./modal-servidor.component.scss']
})
export class ModalServidorComponent implements OnInit {
  registerForm: FormGroup;
  generos: MaestraParametro[] = [];
  tiposDocumento: MaestraParametro[] = [];
  unidadesOrganicas: UnidadOrganicaCombo[] = [];
  sindicatos: MaestraParametro[] = [];
  tiposAsignacion: MaestraParametro[] = [];
  listaEncargados: any[] = [];
  listaPuestos: any[] = [];
  listaPuestosFiltrados: any[] = [];
  TIPO_ASIGNACION_ENCARGADO: number = 2;

  constructor(
    private matDialog: MatDialogRef<ModalServidorComponent>,
    private fb: FormBuilder,
    private toastService: ToastService,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private UnidadOrganicaRepository: UnidadOrganicaRepository
  ) { }

  ngOnInit(): void {
    this.cargarCombos();
    this.initializeForm();
  }

  get f() { return this.registerForm.controls; }

  cargarCombos() {
    const getGeneros = this.maeParametroRepository.getMaestraParametro('PER_SEXO');
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getUndOrganicaCbo = this.UnidadOrganicaRepository.getUnidadOrganicaCbo();
    const getSindicatos = this.maeParametroRepository.getMaestraParametro('PER_SINDICATO');
    const getTipoAsignacion = this.maeParametroRepository.getMaestraParametro('TIPO_ASIGNACION_SERVIDOR_CIVIL');
    const getPuestos = this.UnidadOrganicaRepository.getPuestos();

    forkJoin([getGeneros, getTipoDocumento, getUndOrganicaCbo, getSindicatos, getTipoAsignacion, getPuestos])
      .subscribe(
        (results ) => {
          this.generos = results[0];
          this.tiposDocumento = results[1];
          this.unidadesOrganicas = results[2];
          this.sindicatos = results[3];
          this.tiposAsignacion = results[4];
          this.listaPuestosFiltrados = this.listaPuestos = results[5];
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      apellidoPaterno: ['', [Validators.required]],
      apellidoMaterno: ['', [Validators.required]],
      nombres: ['', [Validators.required]],
      sexo: ['', [Validators.required]],
      fechaNacimiento: ['', [Validators.required]],
      correoInstitucional: ['', [Validators.required]],
      tipoDocumento: ['', [Validators.required]],
      numeroDocumento: ['', [Validators.required]],
      organoId: ['', [Validators.required]],
      sindicato: ['', [Validators.required]],
      tipoAsignacion: ['', [Validators.required]],
      personaAsignada: ['', [Validators.required]],
      puestoDescripcion: ['', [Validators.required]],
      fechaInicio: ['', [Validators.required]]
    });

    setTimeout(() => {
      const input = document.getElementById('fechaNacimiento');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e?.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e?.keyCode);
      };

      const fechaInicio = document.getElementById('fechaInicio');


      fechaInicio.setAttribute('maxlength', '10');


      fechaInicio.onkeydown = (e: any) => {
        return this.isNumeric(fechaInicio, e.keyCode);
      };


      fechaInicio.onkeyup = (e: any) => {
        this.validateDateFormat(fechaInicio, e.keyCode);
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

  actualizarEncargados() {
    this.registerForm.get('personaAsignada').setValue('');

    const idUO = this.registerForm.get('organoId').value;
    const tipoAsignacion = this.registerForm.get('tipoAsignacion').value;
    if (idUO && tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      this.servidoresRepository.listarPersonasParaPuesto(idUO, tipoAsignacion)
        .subscribe(x => {
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
    this.actualizarEncargados();
  }

  onChangePuesto($event: any) {
    let valor = $event.target.value.trim().toLowerCase();
    this.listaPuestosFiltrados = this.listaPuestos.filter(x => x.descripcion.toLowerCase().includes(valor));
  }

  onSelectionChangePuesto(idPuesto: number) {
    let seleccionado = this.listaPuestos.find(x => x.id === idPuesto);
    if (seleccionado) {
      this.registerForm.get('puestoDescripcion').setValue(seleccionado.descripcion);
    }
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  registrar() {
    this.registerForm.markAllAsTouched();
    // if (this.registerForm.valid) {
      const datos: DatosPersonalesServidorCivil = this.registerForm.value;
      this.servidoresRepository.agregarServidor(datos)
        .subscribe(x => {
          this.cerrar(true);
        },
        (err) => this.toastService.showToast(err, 'danger'));
    // }
  }
}
