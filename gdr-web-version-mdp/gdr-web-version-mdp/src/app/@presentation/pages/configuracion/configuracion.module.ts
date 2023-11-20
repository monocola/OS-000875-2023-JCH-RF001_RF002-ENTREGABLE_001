import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ConfiguracionComponent } from './configuracion.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ConfiguracionRoutingModule } from './configuracion-routing.module';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbLayoutModule,
  NbCardModule,
  NbDatepickerModule,
  NbTabsetModule,
  NbActionsModule,
  NbAutocompleteModule, NbCheckboxModule

} from '@nebular/theme';
import { AsignarEntidadComponent } from './asignar-entidad/asignar-entidad.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { MatDialogModule } from '@angular/material/dialog';
import { ModalNotificarComponent } from './modal-notificar/modal-notificar.component';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatTableModule } from '@angular/material/table';
import { ModalNotificarComponentRector } from './modal-notificar-rector/modal-notificar-rector.component';

@NgModule({
  declarations: [
    ConfiguracionComponent,
    AsignarEntidadComponent,
    ModalNotificarComponent,
    ModalNotificarComponentRector
  ],
  imports: [
    CommonModule,
    ConfiguracionRoutingModule,
    CommonComponentsModule,
    NbSelectModule,
    ReactiveFormsModule,
    NbButtonModule,
    NbFormFieldModule,
    NbInputModule,
    NbIconModule,
    MatDividerModule,
    NgxTrimDirectiveModule,
    MatTabsModule,
    MatPaginatorModule, MatButtonModule, NbDatepickerModule,
    MatAutocompleteModule, NbTabsetModule, MatDialogModule,
    MatIconModule, FormsModule, NbCardModule, NbLayoutModule,
    NbActionsModule, NbAutocompleteModule, MatCheckboxModule, MatTableModule, NbCheckboxModule
  ]
})
export class ConfiguracionModule { }
