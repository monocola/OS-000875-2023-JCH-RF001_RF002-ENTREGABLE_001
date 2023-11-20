import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { CreacionBaseService } from '../creacion-base.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-step6',
  templateUrl: './step6.component.html',
})
export class Step6Component implements OnInit {
  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    public authRepository: AuthenticationRepository
  ) {}

  listaDeInformes = [];
  informesSeleccionados = [];
  const = Const;
  ngOnInit(): void {
    this.f.informes.valueChanges.subscribe((res) => {
      this.informesSeleccionados = res;
    });
  }

  get f() {
    return this.helperService.form6.controls;
  }

  updateInformes(event) {
    if (event) {
      this.basesService
      .getInformes(event)
      .toPromise()
      .then((res) => {
        this.listaDeInformes = [];
        this.listaDeInformes = res;
      }).catch ((error: any) => {
        this.listaDeInformes = [];
      });
    }
  }

  setItemsSelected(event: any[]) {
    this.informesSeleccionados = [...event];
    this.f.informes.patchValue(event);
  }

  showPDFSelected(e) {
    this.basesService
      .getPDF(e.value, e.tipoInformeId, this.helperService.idBase)
      .subscribe((res) => {
        const base64Data = res;
        this.dialog.open(FileVisualizerComponent, {
          data: {
            base64String: base64Data,
            filename: e.description,
            extension: 'pdf',
          },
        });
      });
  }
}
