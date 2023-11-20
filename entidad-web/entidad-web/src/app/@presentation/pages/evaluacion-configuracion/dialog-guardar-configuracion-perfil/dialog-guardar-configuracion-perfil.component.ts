import { Router } from '@angular/router';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ConfiguracionReqMinRepository } from 'src/app/@domain/repository/configuracion-req-min.repository';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Component, OnInit, Inject } from '@angular/core';

@Component({
  selector: 'serv-talento-dialog-guardar-configuracion-perfil',
  templateUrl: './dialog-guardar-configuracion-perfil.component.html',
  styleUrls: ['./dialog-guardar-configuracion-perfil.component.scss']
})
export class DialogGuardarConfiguracionPerfilComponent implements OnInit {

  constructor(
    private matDialogRef: MatDialogRef<DialogGuardarConfiguracionPerfilComponent>,
    private configuracionReqMinService: ConfiguracionReqMinRepository,
    private toast: ToastService,
    private router: Router,
    @Inject(MAT_DIALOG_DATA) public data
  ) { }

  ngOnInit(): void { }

  dismiss(result: boolean): void {
    this.matDialogRef.close(true);
  }

  confirmar(): void {
    let configuracionPerfilDTO: any;
    configuracionPerfilDTO = {
      configuracionPerfilDTO: {
        listConfigPerfFormacCarrera: this.data.listConfigPerfFormacCarrera,
        listConfigPerfFormacEspecial: this.data.listConfigPerfFormacEspecial,
        listConfigPerfFormacEspecif: this.data.listConfigPerfFormacEspecif,
        listConfigPerfExpeLaboral: this.data.listConfigPerfExpeLaboral,
        listConfigPerfOtrosRequisitos: this.data.listConfigPerfOtrosRequisitos,
        listConfigPerfDeclaraJurada: this.data.listConfigPerfDeclaraJurada,
        listaConfigPerfPesoSeccion: this.data.listaConfigPerfPesoSeccion,
        listaConfigOtrosGrados: this.data.listaConfigOtrosGrados,
        listaConfigInvestigacion: this.data.listaConfigInvestigacion,
        listaConfigPublicacion: this.data.listaConfigPublicacion,
        listaConfigEspecifInvestigacion: this.data.listaConfigEspecifInvestigacion,
        listaConfigEspecifPublicacion: this.data.listaConfigEspecifPublicacion,
        configPerfiles: this.data.configPerfiles,
      }
    };

    this.configuracionReqMinService.setConfiguracionPerfil(configuracionPerfilDTO).subscribe((mc) => {
      this.dismiss(true);
      this.router.navigateByUrl('/pages/configuracion-evaluacion');
      this.toast.showToast('Se guardó la configuración con éxito', 'info', 'Éxito');
    });
  }

}
