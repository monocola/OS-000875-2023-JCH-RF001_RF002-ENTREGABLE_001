import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { PerfilRoutingModule } from './perfiles-routing.module';
import { PerfilesComponent } from './perfiles.component';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbPopoverModule,
  NbSelectModule,
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { MatIconModule } from '@angular/material/icon';
import { ReactiveFormsModule } from '@angular/forms';
import { CreacionComponent } from './creacion/creacion.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { CreacionModalComponent } from './creacion-modal/creacion-modal.component';
import { MatDialogModule } from '@angular/material/dialog';

import { LazyLoadedRootComponent } from './helperComponent';
import { DuplicarModalComponent } from './duplicar-modal/duplicar-modal.component';
import { RegistroMasivoModalComponent } from './registro-masivo-modal/registro-masivo-modal.component';
import { RegistroMasivoErrorModalComponent } from './registro-masivo-error-modal/registro-masivo-error-modal.component';
import { HelperPerfilesService } from './helperPerfiles.service';

@NgModule({
  declarations: [
    PerfilesComponent,
    CreacionComponent,
    CreacionModalComponent,
    LazyLoadedRootComponent,
    DuplicarModalComponent,
    RegistroMasivoModalComponent,
    RegistroMasivoErrorModalComponent
  ],
  imports: [
    CommonModule,
    PerfilRoutingModule,
    NbButtonModule,
    ReactiveFormsModule,
    MatDividerModule,
    NbSelectModule,
    NbFormFieldModule,
    NbIconModule,
    NbInputModule,
    NbDatepickerModule,
    CommonComponentsModule,
    NbPopoverModule,
    MatDialogModule,
    MatIconModule
  ],
  providers: [
    HelperPerfilesService
  ]
})
export class PerfilesModule {}
