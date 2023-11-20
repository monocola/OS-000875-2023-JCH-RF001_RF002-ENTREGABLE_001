import { ModuleWithProviders, NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  NbIconModule,
  NbThemeModule,
} from '@nebular/theme';
import { CORPORATE_THEME } from './styles/theme.corporate';

@NgModule({
  imports: [
    CommonModule,
    NbIconModule
  ],
  exports: [
    CommonModule,
  ],
  declarations: [],
})
export class ThemeModule {
  static forRoot(): ModuleWithProviders<ThemeModule> {
    return {
      ngModule: ThemeModule,
      providers: [
        ...NbThemeModule.forRoot(
          {
            name: 'default',
          },
          [CORPORATE_THEME],
        ).providers,
      ],
    };
  }
}
