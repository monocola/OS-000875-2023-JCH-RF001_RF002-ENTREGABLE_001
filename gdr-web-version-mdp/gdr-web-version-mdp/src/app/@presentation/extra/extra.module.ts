import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ExtraRoutingModule } from './extra-routing.module';
import { ExtraComponent } from './extra.component';
import { RouterModule } from '@angular/router';
import { NbButtonModule, NbIconModule, NbLayoutModule, NbMenuModule, NbSidebarModule } from '@nebular/theme';
import { MatMenuModule } from '@angular/material/menu';
import { ThemeModule } from '../@theme/theme.module';
import { HeaderComponent } from './components/header/header.component';


@NgModule({
  declarations: [
    ExtraComponent,
    HeaderComponent
  ],
  imports: [
    CommonModule,
    ExtraRoutingModule,
    RouterModule,
    ThemeModule,
    NbMenuModule,
    NbLayoutModule,
    NbSidebarModule,
    NbButtonModule,
    NbIconModule,
    MatMenuModule,
  ]
})
export class ExtraModule { }
