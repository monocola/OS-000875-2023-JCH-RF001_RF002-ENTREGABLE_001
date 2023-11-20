import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AsignarGestorGdrRoutingModule } from './asignar-gestor-gdr-routing.module';
import {
  NbAutocompleteModule,
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbSelectModule,
  NbCardModule,
  NbTreeGridModule,
  NbDatepickerModule, NbActionsModule, NbToggleModule
} from '@nebular/theme';

import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatDialogModule } from '@angular/material/dialog';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';
import { DetalleGestorGdrComponent } from './detalle.gestor.gdr/detalle-gestor-gdr.component';
import { NotificarGdrComponent } from './notificar-gdr/notificar-gdr.component';
import { ThemeModule } from 'src/app/@presentation/@theme/theme.module';

@NgModule({
  declarations: [
    DetalleGestorGdrComponent,
    NotificarGdrComponent,
  ],
  imports: [
    CommonModule,
    AsignarGestorGdrRoutingModule,
    ThemeModule,
    NbButtonModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    CommonComponentsModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbAutocompleteModule,
    NgxTrimDirectiveModule,
    MatTabsModule,
    MatIconModule,
    MatPaginatorModule,
    MatButtonModule,
    MatDialogModule,
    MatAutocompleteModule,
    NbCardModule,
    NbTreeGridModule,
    NbDatepickerModule,
    NbActionsModule,
    NbToggleModule
  ]

})
export class AsignarGestorModule { }
