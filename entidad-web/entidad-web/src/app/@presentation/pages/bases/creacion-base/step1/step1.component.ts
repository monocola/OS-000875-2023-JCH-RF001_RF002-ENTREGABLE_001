import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { CreacionBaseService } from '../creacion-base.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-step1',
  templateUrl: './step1.component.html',
  styleUrls: ['./step1.component.scss'],
})
export class Step1Component implements OnInit {
  array2 = [];
  const = Const;
  basesLegales: any[] = [];
  basesLegalesEsp: any[] = [];

  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    public authRepository: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.f.baseSeleccionada.valueChanges.subscribe((res) => {
      this.basesLegales = res;
    });
    this.f.baseSeleccionadaEsp.valueChanges.subscribe((res) => {
      this.basesLegalesEsp = res;
    });
  }

  get f() {
    return this.helperService.form1.controls;
  }

  setItemsSelected(event: any[]) {
    this.basesLegales = [...event];
    this.f.baseSeleccionada.patchValue(event);
  }

  setItemsSelectedEsp(event: any[]) {
    this.basesLegalesEsp = [...event];
    this.f.baseSeleccionadaEsp.patchValue(event);
  }

  showPDFSelected(e) {
    this.basesService.getPDFInforme(e.value).subscribe((res) => {
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

  showPDFSelectedEsp(e) {
    this.basesService.getPDFInforme(e.value).subscribe((res) => {
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
