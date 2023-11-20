import { Component, OnInit } from '@angular/core';
import { ToastService } from '../../@common-components/toast';
import { getBase64 } from '../../../utils/converterFile';
@Component({
  selector: 'gme-web-solicitud-jefe-orh',
  templateUrl: './solicitud-jefe-orh.component.html',
  styleUrls: ['./solicitud-jefe-orh.component.scss']
})
export class SolicitudJefeOrhComponent implements OnInit {

  constructor(
    private toastService: ToastService,
  ) { }

  ngOnInit(): void {
  }

  file: File;
  archivoSeleccionado(events) {
    if (events.target.files.length === 1 ) {
      if ( events.target.files[0].size <= 1101899 ) {
        if ( events.target.files[0].type === 'application/pdf' ) {
          this.file = events.target.files[0];
        } else {
          this.toastService.showToast('Subir un archivo con extensión PDF', 'danger');
        }
      } else {
        this.toastService.showToast('Ingrese un archivo PDF menor a 1 MB', 'danger');
      }
    } else {
      this.toastService.showToast('Solo esta permitido subir un archivo por registro', 'danger');
    }
  }

}
