import { DatePipe } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import {
  AbstractControl,
  FormBuilder,
  FormControl,
  FormGroup,
  ValidationErrors,
  ValidatorFn,
  Validators,
} from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { ActivatedRoute, Router } from '@angular/router';
import { forkJoin } from 'rxjs';
import { MaestraParametro } from 'src/app/@data/model/maestra-parametro';
import { PuestoUoServidorCivil } from 'src/app/@data/model/puesto';
import { DatosPersonalesServidorCivil } from 'src/app/@data/model/servidores-civiles';
import { UnidadOrganicaCombo } from 'src/app/@data/model/unidadOrganicaCombo';
import { MaestraParametroRepository } from 'src/app/@domain/repository/maestra-parametro.repository';
import { ServidoresRepository } from 'src/app/@domain/repository/servidores.repository';
import { UnidadOrganicaRepository } from 'src/app/@domain/repository/unidad-organica.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EditarPuestoComponent } from '../editar-puesto/editar-puesto.component';
import moment from 'moment/moment';

@Component({
  selector: 'serv-talento-editar-servidor',
  templateUrl: './editar-servidor.component.html',
  styleUrls: ['./editar-servidor.component.scss'],
})
export class EditarServidorComponent implements OnInit {
  registerForm: FormGroup;
  registerFormPuesto: FormGroup;
  generos: MaestraParametro[] = [];
  tiposDocumento: MaestraParametro[] = [];
  unidadesOrganicas: UnidadOrganicaCombo[] = [];
  sindicatos: MaestraParametro[] = [];
  tiposRegimen: MaestraParametro[] = [];

  listaEncargados: any[] = [];
  listaPuestos: any[] = [];
  listaPuestosFiltrados: any[] = [];
  listaPuestosTablas: PuestoUoServidorCivil[] = [];
  listMotivosEncarturas: any[] = [];
  columnasTablaPuestos: TableColumn[];

  datos: DatosPersonalesServidorCivil = {};

  idServidor: number;
  uoId: number;
  detUoId: number;
  regimenId: number;
  TIPO_ASIGNACION_ENCARGADO: number = 2;
  fechaNac: any;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toastService: ToastService,
    private datePipe: DatePipe,
    private maeParametroRepository: MaestraParametroRepository,
    private servidoresRepository: ServidoresRepository,
    private unidadOrganicaRepository: UnidadOrganicaRepository,
    private route: ActivatedRoute,
    private router: Router,
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    this.route.queryParams.subscribe((params) => {
      this.idServidor = Number(params.id);
      this.uoId = Number(params.uoId);
      this.detUoId = Number(params.detUoId);
      this.regimenId = Number(params.regimenId);
      console.info(this.regimenId);
      this.cargarValores();
    });
  }

  get f() {
    return this.registerForm.controls;
  }
  get fp() {
    return this.registerFormPuesto.controls;
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      apellidoPaterno: [{ value: '', disabled: true }],
      apellidoMaterno: [{ value: '', disabled: true }],
      nombres: [{ value: '', disabled: true }],
      genero: [{ value: '', disabled: true }],
      fechaNacimiento: [{ value: '', disabled: true }],
      tipoDocumentoIdentidad: [{ value: '', disabled: true }],
      numeroDocumentoIdentidad: [{ value: '', disabled: true }],
      correo: ['', [Validators.required, Validators.email]],
      sindicato: ['', [Validators.required]],
      regimenLaboralId: ['', [Validators.required]],
    });

    this.registerFormPuesto = new FormGroup(
      {
        organo: new FormControl('', [Validators.required]),
        puestoId: new FormControl('', [Validators.required]),
        fechaInicio: new FormControl('', [Validators.required]),
        personaAsignada: new FormControl('', null),
        motivoId: new FormControl('', [Validators.required]),
      },
      {
        validators: this.validarMotivo,
      }
    );
  }

  onTabChanged(event) {
    if (event.index === 1) {
      setTimeout(() => {
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

  initializeColumns() {
    this.columnasTablaPuestos = [
      {
        name: 'Nro',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Puesto',
        dataKey: 'descripcionPuesto',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Órgano / UO / Sub UO',
        dataKey: 'siglaUO',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Tipo de Asignación',
        dataKey: 'descTipoAsignacion',
        position: 'left',
        isSortable: true,
      },
      { name: 'Estado', dataKey: 'estado', position: 'left', isSortable: true },
      {
        name: 'Fecha Inicio',
        dataKey: 'fechaInicioTexto',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Fecha Cese',
        dataKey: 'fechaCeseTexto',
        position: 'left',
        isSortable: true,
      },
    ];
  }

  cargarValores() {
    const getGeneros = this.maeParametroRepository.getMaestraParametro(
      'PER_SEXO'
    );
    const getTipoDocumento = this.servidoresRepository.getTiposDocumento();
    const getUndOrganicaCbo = this.unidadOrganicaRepository.getUnidadOrganicaCbo();
    const getSindicatos = this.maeParametroRepository.getMaestraParametro(
      'PER_SINDICATO'
    );
    const getDatos = this.servidoresRepository.getDatosPersonalesRegimen(
      this.detUoId,
      this.idServidor,
      this.regimenId
    );
    const getTipoRegimen = this.maeParametroRepository.getMaestraParametro(
      'TIPO_REGIMEN_LABORAL'
    );
    const getMotivoEncargadura = this.maeParametroRepository.getMaestraParametro(
      'MOTIVO_ENCARGATURA'
    );

    forkJoin([
      getGeneros,
      getTipoDocumento,
      getUndOrganicaCbo,
      getSindicatos,
      getDatos,
      getTipoRegimen,
      getMotivoEncargadura,
    ]).subscribe(
      (results) => {
        this.generos = results[0];
        this.tiposDocumento = results[1];
        this.unidadesOrganicas = results[2];
        this.sindicatos = results[3];
        this.cargarDatosPersonales(results[4]);
        this.tiposRegimen = results[5];
        this.listMotivosEncarturas = results[6];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  compareById(v1, v2): boolean {
    return v1 === v2;
  }

  cargarDatosPersonales(datos: DatosPersonalesServidorCivil) {
    this.datos = datos;

    setTimeout(() => {
      this.registerForm
        .get('apellidoPaterno')
        .setValue(this.datos.apellidoPaterno);
      this.registerForm
        .get('apellidoMaterno')
        .setValue(this.datos.apellidoMaterno);
      this.registerForm.get('nombres').setValue(this.datos.nombres);
      this.registerForm.get('genero').patchValue(this.datos.genero);
      let fechaN =
        this.datos.fechaNacimiento == null ? null : this.datos.fechaNacimiento;
      if (fechaN) {
        this.fechaNac = moment(fechaN, 'DD/MM/YYYY').toDate();
      }
      this.registerForm.get('fechaNacimiento').setValue(this.fechaNac);
      this.registerForm.get('correo').setValue(this.datos.correoInstitucional);
      this.registerForm
        .get('tipoDocumentoIdentidad')
        .patchValue(this.datos.tipoDocumento);
      this.registerForm
        .get('numeroDocumentoIdentidad')
        .setValue(this.datos.numeroDocumento);
      this.registerForm
        .get('sindicato')
        .patchValue(Number(this.datos.sindicato));
      this.registerForm
        .get('regimenLaboralId')
        .patchValue(
          this.tiposRegimen.find(
            (x) => x.valorTexto === this.datos?.regimenLaboral
          )?.codigoNumero
        );

      this.registerFormPuesto.get('organo').patchValue(this.uoId);
      this.unidadOrganicaRepository
        .getPuestos({
          unidadOrganicaID: this.uoId,
          nombrePuesto: '',
          esJefe: '',
          puestoId: '',
        })
        .subscribe((res) => (this.listaPuestos = res));
      this.actualizarTablaPuestos(this.uoId);
    }, 250);
  }

  actualizar() {
    this.registerForm.markAllAsTouched();
    if (this.registerForm.valid) {
      this.datos.correoInstitucional = this.registerForm.get('correo').value;
      this.datos.sindicato = this.registerForm.get('sindicato').value;
      this.datos.regimenLaboralId = this.registerForm.get(
        'regimenLaboralId'
      ).value;
      this.servidoresRepository.actualizarServidor(this.datos).subscribe(
        (x) => {
          this.toastService.showToast(
            'Se realizó la actualización exitosamente',
            'success'
          );
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
    }
  }

  encargarPuesto() {
    this.registerFormPuesto.markAllAsTouched();
    if (this.registerFormPuesto.valid) {
      const uoId: number = this.registerFormPuesto.get('organo').value;
      const datos: PuestoUoServidorCivil = {
        uoId: uoId,
        personaId: this.idServidor,
        puestoId: this.registerFormPuesto.get('puestoId').value,
        fechaInicio: this.registerFormPuesto.get('fechaInicio').value,
        tipoAsignacion: `${this.TIPO_ASIGNACION_ENCARGADO}`,
        idDetUOPersonaAsingada: Number(
          this.registerFormPuesto.get('personaAsignada').value
        ),
        motivoId: this.registerFormPuesto.get('motivoId').value,
      };

      this.servidoresRepository.agregarPuesto(datos).subscribe(
        (x) => {
          this.actualizarTablaPuestos(uoId);
          this.registerFormPuesto.reset();
          this.registerFormPuesto.get('organo').setValue(uoId);

          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
        },
        (err) => this.toastService.showToast(err, 'danger')
      );
    }
  }

  editPuesto(item: PuestoUoServidorCivil) {
    this.dialog
      .open(EditarPuestoComponent, {
        disableClose: true,
        data: {
          ...item,
          personaId: this.idServidor,
        },
      })
      .afterClosed()
      .subscribe((procesado) => {
        if (procesado) {
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );
          const uoId: number = this.registerFormPuesto.get('organo').value;
          this.actualizarTablaPuestos(uoId);
          this.router.navigate(['pages/servidoresCiviles']);
        }
      });
  }

  removePuesto(item: any) {
    // this.dataToDelete = item;
    // this.dialog.open(ModalEliminarComponent);
  }

  actualizarEncargados() {
    this.registerFormPuesto.get('personaAsignada').setValue('');

    const idUO = this.registerFormPuesto.get('organo').value;
    const puestoId = this.registerFormPuesto.get('puestoId').value;
    // this.servidoresRepository.listarPersonasParaPuesto(idUO, puestoId)
    this.servidoresRepository
      .listarPersonasParaPuesto(idUO, puestoId)
      .subscribe((x) => {
        this.listaEncargados = x;
        
        if(this.listaEncargados.length > 0) {
          this.registerFormPuesto.get('personaAsignada').setValidators([Validators.required, Validators.minLength(9)]);
          this.registerFormPuesto.get('personaAsignada').updateValueAndValidity();
        } else {
          this.registerFormPuesto.get('personaAsignada').clearValidators();
          this.registerFormPuesto.get('personaAsignada').updateValueAndValidity();
        }

      });
  }

  actualizarTablaPuestos(uoId: number) {
    this.servidoresRepository
      .getHisotrialPuestos(this.idServidor, uoId)
      .subscribe((x) => {
        this.listaPuestosTablas = x.map((p) => {
          //   p.estado === 'Activo' ? p.colorEstado = '#0d88bc' : p.colorEstado = '#9a9a9a';
          p.estadoRegistro === '1'
            ? (p.colorEstado = '#00BFA6')
            : p.estadoRegistro === '2'
            ? (p.colorEstado = '#606D7D')
            : p.estadoRegistro === '3'
            ? (p.colorEstado = '#9a9a9a')
            : '#9a9a9a';

          return {
            ...p,
            fechaInicioTexto: this.datePipe.transform(
              p.fechaInicio,
              'dd/MM/yyyy'
            ),
            fechaCeseTexto: this.datePipe.transform(p.fechaCese, 'dd/MM/yyyy'),
            descripcionTipoAsignacion:
              Number(p.tipoAsignacion) === 1 ? 'Principal' : 'Encargatura',
          };
        });
      });
  }

  onSelectionChangeUO(uoId?: number) {
    this.registerFormPuesto.get('puestoId').setValue('');
    this.registerFormPuesto.get('personaAsignada').setValue('');

    if (this.registerFormPuesto.get('organo').valid) {
      this.actualizarEncargados();
      this.unidadOrganicaRepository
        .getPuestos({
          unidadOrganicaID: uoId,
          nombrePuesto: '',
          esJefe: '',
          puestoId: '',
        })
        .subscribe((res) => (this.listaPuestos = res));
      this.actualizarTablaPuestos(this.registerFormPuesto.get('organo').value);
    } else {
      this.listaEncargados = [];
      this.listaPuestos = [];
    }
  }

  getDataExport() {
    return null;
  }

  public validarMotivo: ValidatorFn = (
    content: AbstractControl
  ): ValidationErrors | null => {
    if (content.get('motivoId').value === 1) {
      const personId = content.get('personaAsignada').value;
      let person = this.listaEncargados.find(
        (encar) => encar.detalleuoId === personId
      );
      if (person?.puestoFechaCese || !person) {
        return null;
      } else {
        return { noCese: true };
      }
    }
    return null;
  }
}
