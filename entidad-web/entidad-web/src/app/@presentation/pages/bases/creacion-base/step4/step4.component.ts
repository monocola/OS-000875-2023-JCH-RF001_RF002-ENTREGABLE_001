import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { CreacionBaseService } from '../creacion-base.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';
import { Subject } from 'rxjs';

@Component({
  selector: 'serv-talento-step4',
  templateUrl: './step4.component.html',
  styleUrls: ['./step4.component.scss'],
})
export class Step4Component implements OnInit {
  currentRow: any;
  stateInputChange$: Subject<string> = new Subject<string>();
  informes: any[] = [];
  filtro: any[] = [];
  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    public authRepository: AuthenticationRepository
  ) {}
  displayedColumns: string[] = [
    'position',
    'informe',
    'name',
    'weight',
    'symbol',
  ];
  criteriosDeEvaluacionSeleccionados = [];
  listaEvaluacionesColumns = [];
  const = Const;
  ngOnInit(): void {
    this.initializeColumns();
  }

  get f() {
    return this.helperService.form4.controls;
  }

  initializeColumns() {
    this.listaEvaluacionesColumns = [
      {
        name: 'EVALUACION',
        dataKey: 'evaluacion',
        position: 'left',
        isSortable: true,
        width: '40%',
      },
      {
        name: 'PESO (%)',
        dataKey: 'peso',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'PUNTAJE MÍNIMO',
        dataKey: 'puntajeMinino',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
      {
        name: 'PUNTAJE MÁXIMO',
        dataKey: 'puntajeMaximo',
        position: 'left',
        isSortable: true,
        width: '20%',
      },
    ];
  }

  setItemsSelected(event: any[]) {
    this.criteriosDeEvaluacionSeleccionados = [...event];
    this.f.criterioDeEvaluacion.patchValue(event);
  }

  showPDFSelected(e) {
    this.basesService
      .getPDF(
        e.informeDetalleId,
        Const.CRITERIO_EVALUACION,
        this.helperService.idBase
      )
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
  getTitle(informeId: string) {
    if (informeId) {
      return this.helperService.criteriosDeEvaluacion.find(
        (informe) => informe.value === informeId
      ).description;
    }
  }

  onNamengModelDataChanged(newvalue: any, index: any) {
    this.helperService.listaDeEvaluaciones[index].informeDetalleId = newvalue;
    (async () => {
      await this.delay(500);

      this.informes = [];
    })();
    this.helperService.cambiosEvaluacion = true;
  }

  delay(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  clear(index: any) {
    this.helperService.listaDeEvaluaciones[index].informeDetalleId = null;
    this.helperService.cambiosEvaluacion = true;
  }

  filtrar(e) {
    this.filtro = this.helperService.criteriosDeEvaluacion;
    this.informes = this.filtro.filter((obj) =>
      obj.description.toLowerCase().includes(e.toLowerCase())
    );
  }
}
