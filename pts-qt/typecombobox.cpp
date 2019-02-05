#include "typecombobox.h"

TypeComboBox::TypeComboBox(QWidget *parent) :
    QComboBox(parent)
{
    all = TypeComboBox::allTypes();
    setModel(&model);

    for (auto const &t: all) {
        model.appendRow(new QStandardItem(t.humanDisplay().toQString()));
    }

    connect(this,
            static_cast<void (QComboBox::*)(int index)>(&QComboBox::currentIndexChanged),
            this,
            [this]()
    {
        emit typeSelected(currentType());
    });

    this->setCurrentIndex(5);
}

pts::Type TypeComboBox::currentType()
{
    auto index = currentIndex();

    if (index >= 0 && index < int(all.size())) {
        return all.at(uintptr_t(index));
    }

    return pts::Type();
}

std::vector<pts::Type> TypeComboBox::allTypes()
{
    std::vector<pts::Type> types;
    types.push_back(pts::Type::parse("u8"));
    types.push_back(pts::Type::parse("i8"));
    types.push_back(pts::Type::parse("u16"));
    types.push_back(pts::Type::parse("i16"));
    types.push_back(pts::Type::parse("u32"));
    types.push_back(pts::Type::parse("i32"));
    types.push_back(pts::Type::parse("u64"));
    types.push_back(pts::Type::parse("i64"));
    types.push_back(pts::Type::parse("u128"));
    types.push_back(pts::Type::parse("i128"));
    return types;
}
