import { Component, OnInit } from '@angular/core';
import { faSignOutAlt } from '@fortawesome/free-solid-svg-icons';
import { AuthenticationRepository } from '../../../@domain/repository/authentication.repository';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { DatePipe } from '@angular/common';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Ciclos } from '../../../@data/model/ciclos';
import { Sort } from '@angular/material/sort';
import { getMiliseconds, sortDataTableComponent } from '../../../utils/general';
import { OrganigramaRepository } from '../../../@domain/repository/organigrama.repository';
import { forkJoin } from 'rxjs';
import { Puestos } from '../../../@data/model/puesto';
import { MaestraParametroRepository } from '../../../@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from '../../../@data/model/maestra-parametro';
import { Utils } from '../../../utils/utils';
import { ToastService } from '../../@common-components/toast';
import { getBase64 } from '../../../utils/converterFile';
import { Const } from '../../../@data/services/const';

@Component({
  selector: 'serv-talento-profile',
  templateUrl: './profile.component.html',
  styleUrls: ['./profile.component.scss']
})
export class ProfileComponent implements OnInit {
  constructor(
    private authenticationService: AuthenticationRepository,
    private fb: FormBuilder,
    private datePipe: DatePipe,
    private organigramaService: OrganigramaRepository,
    private maeParametro: MaestraParametroRepository,
    private toast: ToastService
  ) {
    this.frm = this.fb.group({
    telefono : new FormControl(null, [
      Validators.pattern(/[0-9]$/),
      Validators.minLength(7),
      Validators.maxLength(9)]
    ),
    correoAlt : new FormControl(null, Validators.email),
    sindicato : new FormControl(null, Validators.required),
  });
}

  url =
    'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT3iyWiS5Tu20L5S91HBmKnuRTNT8DffM8MCg&usqp=CAU';
  profile = this.authenticationService.getCurrentUserValue;
  iconOut = faSignOutAlt;
  nombreRol = '';

  logoProfile;
  frm: FormGroup;
  ordersTableColumns: TableColumn[];
  dataToEdit: Ciclos = null;
  dataToDelete: Ciclos = null;
  holderText = 'Buscar por Estado';

  lstPuestos: Puestos[] = [];
  datosPersonales: any;
  generoList: MaestraParametro[] = [];
  sindicatoList: MaestraParametro[] = [];

  fechaNac: any;
  correoInst;
  correoAlt;
  regimenLab;
  genero;
  sindicato;

  generoDisabled = true;
  fechaNacDisabled = true;
  correoInstDisabled = true;
  regimenLabDisabled = true;

  nomArchivo = '';
  imgData64 = '';
  imgFile: File = null;
  flagUpdatePhoto = 0;
  entidad;
  imgEntidad = './assets/images/logo.png';

  get f() {
    return this.frm.controls;
  }

  ngOnInit(): void {
    // this.initialFrom();
    this.loadCombox();
    this.initializeColumns();
    setTimeout(() => {
      this.nombreRol = JSON.parse(sessionStorage.getItem('roles')).nombreRol;
      if ( this.entidad !== null  && this.entidad.logo !== null ) {
        this.logoProfile = Const.API_FILE_SERVER  + this.entidad.logo;
      }
    }, 0);
  }

  loadCombox() {
    const getListPuestos = this.organigramaService.getListPuesto(
      this.profile.entidadId,
      this.profile.personaId
    );

    const getDatosPersonales = this.organigramaService.getDatosPersona(
      this.profile.entidadId,
      this.profile.personaId
    );

    const getSindicatos = this.maeParametro.getMaestraParametro(
      'PER_SINDICATO'
    );

    const getGenero = this.maeParametro.getMaestraParametro('PER_SEXO');

    forkJoin([
      getListPuestos,
      getDatosPersonales,
      getSindicatos,
      getGenero,
    ]).subscribe((results) => {
      console.log(results);
      this.lstPuestos = results[0];
      this.datosPersonales = results[1];
      this.sindicatoList = results[2];
      this.generoList = results[3];
      this.initialFrom();
    });
  }

  initializeColumns() {
    this.ordersTableColumns = [
      {
        name: 'Nro',
        dataKey: 'correlativo',
        position: 'left',
        isSortable: true,
      },
      { name: 'Puesto', dataKey: 'puesto', position: 'left', isSortable: true },
      {
        name: 'Siglas del O / UO',
        dataKey: 'siglaUO',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Tipo',
        dataKey: 'tipoAsignacion',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Segmento',
        dataKey: 'segmento',
        position: 'left',
        isSortable: true,
      },
      { name: 'Rol', dataKey: 'rol', position: 'left', isSortable: true },
      { name: 'Estado', dataKey: 'estado', position: 'left', isSortable: true },
    ];
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.lstPuestos);
  }

  editSede(item: Ciclos) {
    this.dataToEdit = item;
  }

  removeSede(item: Ciclos) {
    this.dataToDelete = item;
  }

  getDataExport() {
    return null;
  }

  initialFrom() {
    this.frm = this.fb.group({
      sindicato : new FormControl(null, Validators.required),
      telefono : new FormControl(null, [
        Validators.required,
        Validators.pattern(/[0-9]$/),
        Validators.minLength(7),
        Validators.maxLength(9)]
      ),
      correoAlt : new FormControl(null, [
        Validators.required,
        Validators.email]
      ),
    });

    if (this.datosPersonales.fechaNacimiento) {
      this.fechaNac = this.datosPersonales.fechaNacimiento
    } else { this.fechaNac = this.datePipe.transform(new Date(), 'yyyy-MM-dd'); }

    if ( this.datosPersonales.genero) {
      this.genero = this.generoList.find(item => item.codigoTexto === this.datosPersonales.genero).valorTexto;
    } else { this.genero = this.datosPersonales.genero; }

    this.correoInst = this.datosPersonales.correoInstitucional;
    this.regimenLab = this.datosPersonales.regimenLaboral;

    if ( this.datosPersonales.urlFoto ) {
      this.logoProfile = Const.API_FILE_SERVER + this.datosPersonales.urlFoto;
    } else { this.logoProfile = this.url; }

    this.frm.patchValue({
      telefono: this.datosPersonales.telefono,
      correoAlt: this.datosPersonales.correoAlternativo,
      sindicato: Number(this.datosPersonales.sindicato),
    });
  }

  modProfile() {
    const body = this.validaCampos();
    if (this.frm.valid) {
      this.organigramaService.updateDatosPersonales(body).subscribe((res) => {
        if (res.status.success) {
          this.loadCombox();
          this.toast.showToast('Se actualizó con Exito', 'success', 'Atención');
        } else {
          this.toast.showToast(
            res.status.error.messages[0],
            'danger',
            'Atención'
          );
        }
      });
    } else {
      this.toast.showToast('Debe completar los campos', 'danger');
    }
  }

  validaCampos() {
    const personaId = this.profile.personaId;
    const entidadId = this.profile.entidadId;

    let telefono = null;
    if (this.frm.value.telefono || this.frm.value.telefono > 0) {
      telefono = this.frm.value.telefono;
    } else {
      this.toast.showToast('Debe completar el campo telefono', 'danger');
    }

    let correoAlt = null;
    if (this.frm.value.correoAlt || this.frm.value.correoAlt > 0) {
      correoAlt = this.frm.value.correoAlt;
    } else {
      this.toast.showToast(
        'Debe completar el campo correo Alternativo',
        'danger'
      );
    }

    let sindicato = null;
    // if ( this.frm.value.sindicato || this.frm.value.sindicato > 0 ) {
    if (this.frm.value.sindicato === 0 || this.frm.value.sindicato === 1) {
      sindicato = this.frm.value.sindicato;
    } else {
      this.toast.showToast('Debe completar el campo sindicato', 'danger');
    }
    return {
      entidadId: entidadId,
      personaId: personaId,
      telefono: telefono,
      correoAlterno: correoAlt,
      sindicatoFlag: sindicato,
      foto: {
        flag: this.flagUpdatePhoto,
        fileBase64: this.imgData64 ? this.imgData64.split('base64,')[1] : null,
        fileName: this.imgFile
          ? getMiliseconds() +
            '.' +
            this.imgFile.name.split('.')[
              this.imgFile.name.split('.').length - 1
            ]
          : null,
      },
    };
  }

  clearImage() {
    this.logoProfile = null;
    this.imgData64 = '';
    this.imgFile = null;
  }

  fileLogoEvent(fileInput) {
    if (fileInput.target.files[0].size / (1024 * 1024) > 0.5) {
      this.toast.showToast('El archivo excede el tamaño de 500Kb', 'danger');
    } else {
      const extension = fileInput.target.files[0].name.split('.')[
        fileInput.target.files[0].name.split('.').length - 1
      ];
      const extensionesPermitidas = [
        'jpg',
        'JPG',
        'png',
        'PNG',
        'JPEG',
        'jpeg',
      ];
      if (extensionesPermitidas.includes(extension)) {
        this.imgFile = fileInput.target.files[0] as File;
        this.nomArchivo = this.imgFile.name;
        getBase64(this.imgFile).then((data: string) => {
          this.flagUpdatePhoto = 0;
          this.logoProfile = null;
          this.imgData64 = data;
        });
      } else {
        this.imgFile = null;
        this.toast.showToast(
          'Solo están permitidos los archivos jpg, png, jpeg',
          'danger'
        );
      }
    }
  }
}
