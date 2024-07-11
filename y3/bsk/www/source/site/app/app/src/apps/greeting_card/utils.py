import os


def render_card(card):
    recipient = card.recipient
    content = card.content
    footer = card.sender.profile.footer
    with open(
        f"{os.path.dirname(os.path.realpath(__file__))}/card_templates/{card.template}",
        "r",
    ) as f:
        rendered = f.read()
    rendered = rendered.replace("[recipient]", recipient)
    rendered = rendered.replace("[content]", content)
    rendered = rendered.replace("[footer]", footer)

    return rendered
