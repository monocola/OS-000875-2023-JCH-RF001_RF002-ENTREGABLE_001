import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { UnidadOrganicaRepository } from '../../../../@domain/repository/unidad-organica.repository';
import { DatePipe } from '@angular/common';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'gme-web-modal-servidor',
  templateUrl: './modal-servidor.component.html',
  styleUrls: ['./modal-servidor.component.scss'],
})
export class ModalServidorComponent implements OnInit {
  profile = JSON.parse(sessionStorage.getItem('entidad'));
  registerForm: FormGroup;
  generos: MaestraParametro[] = [];
  tiposDocumento: MaestraParametro[] = [];
  unidadesOrganicas: UnidadOrganicaCombo[] = [];
  sindicatos: MaestraParametro[] = [];
  tiposAsignacion: MaestraParametro[] = [];
  listaEncargados: any[] = [];
  listaPuestos: any[] = [];
  listaPuestosFiltrados: any[] = [];
  tiposRegimen: MaestraParametro[] = [];
  tiposOrgano: MaestraParametro[];
  TIPO_ASIGNACION_PRINCIPAL: number = 1;
  TIPO_ASIGNACION_ENCARGADO: number = 2;
  simpleCharacteresAndNumber: string = 'integer';
  numeroDocumentoMaxlength: number = 8;

  constructor(
    private matDialog: MatDialogRef<ModalServidorComponent>,
    private fb: FormBuilder,
    private toastService: ToastService,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private datePipe: DatePipe,
    private authenticationService: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.cargarCombos();
    this.initializeForm();
  }

  get f() {
    return this.registerForm.controls;
  }

  cargarCombos() {
    const getGeneros = this.maeParametroRepository.getMaestraParametro(
      'PER_SEXO'
    );
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    const getSindicatos = this.maeParametroRepository.getMaestraParametro(
      'PER_SINDICATO'
    );
    const getTipoAsignacion = this.maeParametroRepository.getMaestraParametro(
      'TIPO_ASIGNACION_SERVIDOR_CIVIL'
    );
    const getPuestos = this.unidadOrganicaRepository.getPuestos({
      unidadOrganicaID: '',
      puestoId: '',
      nombrePuesto: '',
      esJefe: '',
    });
    const getTipoRegimen = this.maeParametroRepository.getMaestraParametro(
      'TIPO_REGIMEN_LABORAL'
    );
    const getOrganos = this.maeParametroRepository.getMaestraParametro(
      'TIPO_NATURALEZA'
    );
    forkJoin([
      getGeneros,
      getTipoDocumento,
      getUndOrganicaCbo,
      getSindicatos,
      getTipoAsignacion,
      getPuestos,
      getTipoRegimen,
      getOrganos,
    ]).subscribe(
      (results) => {
        this.generos = results[0];
        this.tiposDocumento = results[1];
        this.unidadesOrganicas = results[2];
        this.sindicatos = results[3];
        this.tiposAsignacion = results[4];
        this.listaPuestos = results[5];
        this.tiposRegimen = results[6];
        this.tiposOrgano = results[7];
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
      correoInstitucional: [
        '',
        [
          Validators.required,
          Validators.email,
          Validators.pattern('^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}$'),
        ],
      ],
      tipoDocumento: ['', [Validators.required]],
      numeroDocumento: ['', [Validators.required]],
      organoId: ['', [Validators.required]],
      sindicato: [''],
      // tipoAsignacion: [this.TIPO_ASIGNACION_PRINCIPAL, [Validators.required]],
      personaAsignada: [''],
      // puestoDescripcion: ['', [Validators.required]],
      puestoId: ['', [Validators.required]],
      fechaInicio: ['', [Validators.required]],
      regimenLaboral: ['', [Validators.required]],
      tipoOrgano: '',
    });

    this.registerForm.controls['tipoDocumento'].valueChanges.subscribe(
      (newvalue) => {
        this.registerForm.controls['numeroDocumento'].setValue('');
        if (newvalue === 1) {
          this.numeroDocumentoMaxlength = 8;
          this.simpleCharacteresAndNumber = 'integer';

          this.registerForm.controls['numeroDocumento'].setValidators([
            Validators.required,
            Validators.maxLength(8),
            Validators.minLength(8),
          ]);
        } else {
          this.numeroDocumentoMaxlength = 12;
          this.simpleCharacteresAndNumber = 'noSpecialChars';
          this.registerForm.controls['numeroDocumento'].setValidators([
            Validators.required,
            Validators.minLength(9),
            Validators.maxLength(12),
          ]);
        }
 
        this.registerForm.controls['numeroDocumento'].updateValueAndValidity();
        this.registerForm.controls['numeroDocumento'].markAllAsTouched();
      }
    );

    setTimeout(() => {
      const fechaNacimiento = document.getElementById('fechaNacimiento');
      fechaNacimiento.setAttribute('maxlength', '10');
      fechaNacimiento.onkeydown = (e: any) => {
        return this.isNumeric(fechaNacimiento, e.keyCode);
      };
      fechaNacimiento.onkeyup = (e: any) => {
        this.validateDateFormat(fechaNacimiento, e.keyCode);
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

  actualizarEncargados() {
    this.registerForm.get('personaAsignada').setValue('');
    const idUO = this.registerForm.get('organoId').value;
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
    const control = this.registerForm.get('personaAsignada');
    if (tipoAsignacion === this.TIPO_ASIGNACION_ENCARGADO) {
      control.setValidators(Validators.required);
    } else {
      control.clearValidators();
    }
    control.updateValueAndValidity();
  }

  tipoOrganoChange(tipoOrganoId: number) {
    this.unidadOrganicaRepository.getUnidadOrganicaCbo(tipoOrganoId).subscribe(
      (x) => {
        this.unidadesOrganicas = x;
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  onSelectionChangeUO(uoId?: number) {
    this.unidadOrganicaRepository
      .getPuestos({
        unidadOrganicaID: uoId,
        puestoId: '',
        nombrePuesto: '',
        esJefe: '',
      })
      .subscribe((res) => {
        this.registerForm.get('puestoId').setValue('');
        this.listaPuestos = res;
      });
  }

  onChangePuesto($event: any) {
    let valor = $event.target.value.trim().toLowerCase();
    this.listaPuestosFiltrados = this.listaPuestos.filter((x) =>
      x.descripcion.toLowerCase().includes(valor)
    );
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  registrar() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      const frm = this.registerForm.value;
      let sexo = frm.sexo.toString();
      const body = {
        trace: {
          traceId: 'string',
        },
        payload: {
          servidorCivil: {
            nombres: frm.nombres.toUpperCase(),
            apellidoPaterno: frm.apellidoPaterno.toUpperCase(),
            apellidoMaterno: frm.apellidoMaterno.toUpperCase(),
            sexo: sexo,
            fechaNacimiento: this.datePipe.transform(
              frm.fechaNacimiento,
              'dd/MM/yyyy'
            ),
            correoElectronico: frm.correoInstitucional,
            tipoDocumento: frm.tipoDocumento,
            numeroDocumento: frm.numeroDocumento,
            sindicatoId: frm.sindicato,
            organoId: frm.organoId,
            tipoAsignacion: this.TIPO_ASIGNACION_PRINCIPAL, // frm.tipoAsignacion,
            idDetUOPersonaAsingada: frm.personaAsignada,
            puestoId: frm.puestoId,
            puestoDescripcion: this.listaPuestos.find(
              (x) => x.puestoId === frm.puestoId
            ).nombrePuesto,
            fechaInicio: this.datePipe.transform(frm.fechaInicio, 'dd/MM/yyyy'),
            responsable: 'N',
            // personaAsignada: frm.personaAsignada,
            regimenLaboralId: frm.regimenLaboral,
          },
          entidadId: this.profile.entidadId,
        },
      };
      this.servidoresRepository.agregarServidor(body).subscribe(
        (x) => {
          this.cerrar(true);
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
    }
  }
}
