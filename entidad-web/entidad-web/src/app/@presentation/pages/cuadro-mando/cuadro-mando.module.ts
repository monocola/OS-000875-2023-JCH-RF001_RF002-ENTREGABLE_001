import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { CuadroMandoRoutingModule } from './cuadro-mando-routing.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRippleModule } from '@angular/material/core';
import { MatDividerModule } from '@angular/material/divider';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbTimepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatIconModule } from '@angular/material/icon';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { CuadroMandoComponent } from './cuadro-mando.component';
import { ChartModule } from 'primeng/chart';
import 'chartjs-plugin-labels';
import 'chartjs-plugin-doughnutlabel';
import { GestorComponent } from './gestor/gestor.component';
import { CoordinadorComponent } from './coordinador/coordinador.component';
import { AdminComponent } from './admin/admin.component';
import { ServirComponent } from './servir/servir.component';

@NgModule({
  declarations: [CuadroMandoComponent, GestorComponent, CoordinadorComponent, AdminComponent, ServirComponent],
  imports: [
    ChartModule,
    CommonModule,
    MatTableModule,
    CuadroMandoRoutingModule,
    MatDividerModule,
    MatRippleModule,
    NbButtonModule,
    CommonComponentsModule,
    MatFormFieldModule,
    MatInputModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NbFormFieldModule,
    NbDatepickerModule,
    MatIconModule,
    MatButtonModule,
    MatPaginatorModule,
    MatSortModule,
    NbTimepickerModule,
  ],
})
export class CuadroMandoModule {}
